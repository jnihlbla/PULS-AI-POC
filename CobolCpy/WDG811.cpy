000100 01  SID-WDG811.                                                          
000200*                                 ÅTERSTARTSREGISTER                      
000300*                                 SID-SEGMENT                             
000400*                                 FYSISK NYCKEL: IDSID                    
000500     03 SID-KVLL             PIC S9(4)           COMP.                    
000600*                                 LRECL I ETT VARIABELT RECORD            
000700*                                 LRECL IN A VARIABLE RECORD              
000800     03 SID-IDSID            PIC S9(3)           COMP-3.                  
000900*                                 SIDNUMRERING                            
001000*                                 PAGE NUMBER                             
001100     03 SID-KVLL-BDW         PIC S9(4)           COMP.                    
001200*                                 BLOCK LÄNGD                             
001300*                                 BLOCK DESCRIPTION WORD                  
001400     03 SID-KVLL-RDW         PIC S9(4)           COMP.                    
001500*                                 RECORD LÄNGD                            
001600*                                 RECORD DESCRIPTION WORD                 
001700     03 SID-TEPRTSID         PIC X(9590).                                 
001800*                                 PRINTAD SIDA                            
001900*                                 PRINTED PAGE                            
002000     03 FILLER-FILLER REDEFINES SID-TEPRTSID.                             
002100        05 FILLER            OCCURS 70 TIMES.                             
002200           07 SID-IDPRTRAD   PIC 9(2).                                    
002300*                                 RADNUMMER PÅ EN PRINTAD SIDA            
002400*                                 LINE NUMBER ON AN PRINTED PAGE          
002500           07 SID-TEPRTRAD   PIC X(132).                                  
002600*                                 PRINTAD RAD                             
002700*                                 PRINTED LINE                            
002800        05 FILLER            PIC X(210).                                  
002900*** END OF VILMAII-COPY LENGTH= 9598 BYTES                                
