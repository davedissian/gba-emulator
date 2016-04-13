/* Fetch instructions */

use cpu::ops::Instruction;
use cpu::ops::Cont;
use cpu::decoder;

pub trait Fetcher {
    fn fetch_word(&mut self) -> u8;

    fn fetch_dword(&mut self) -> u16 {
        let l = self.fetch_word();
        let h = self.fetch_word();
        ((l as u16) << 8) | (h as u16)
    }

    fn fetch_instr(&mut self) -> Instruction {
        let instr = self.fetch_word();

        // The decoded instruction is either some partial
        // instruction which needs further data (a single or double word),
        // or a 'done' instruction which is ready to be fed to the execution
        // unit
        match decoder::decode(instr) {
            Cont::Partial8(f)  => f(self.fetch_word()),
            Cont::Partial16(f) => f(self.fetch_dword()),
            Cont::Done(d)      => d,
        }
    }
}
