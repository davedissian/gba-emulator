/* Fetch instructions */

use cpu::ops::Instruction;
use cpu::ops::Cont;
use cpu::decoder;

pub trait Fetcher {
    fn fetch_u8(&mut self) -> u8;
    fn fetch_u16(&mut self) -> u16;

    fn fetch_instr(&mut self) -> Instruction {
        let instr = self.fetch_u8();

        // The decoded instruction is either some partial
        // instruction which needs further data (a single or double word),
        // or a 'done' instruction which is ready to be fed to the execution
        // unit
        match decoder::decode(instr) {
            Cont::Partial8(f)  => f(self.fetch_u8()),
            Cont::Partial16(f) => f(self.fetch_u16()),
            Cont::Done(d)      => d,
        }
    }
}
