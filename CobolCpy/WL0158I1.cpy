000100 01  REQU-WL0158I1.                                                       
000200*                                 REQUEST TO PGM WL0158                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDRT-KEY        PIC X(3).                                    
000600*                                 RETURTERMINAL                           
000700     03 REQU-IDRTLOP-KEY     PIC 9(3).                                    
000800*                                 RETUR TERMINAL LÖPNUMMER                
000900     03 REQU-FLVISA-KEY      PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100     03 REQU-IDDC-RET        PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 REQU-OMSTART-NYCKLAR.                                             
001400*                                 NYCKLAR FÖR OMSTART AV PGM              
001500        05 REQU-IDDC-SPAR    PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700        05 REQU-IDRT-SPAR    PIC X(3).                                    
001800*                                 RETURTERMINAL                           
001900        05 REQU-IDRTLOP-SPAR PIC 9(3).                                    
002000*                                 RETUR TERMINAL LÖPNUMMER                
002100        05 REQU-IDKOLLI-SPAR PIC 9(5).                                    
002200*                                 KOLLINUMMER                             
002300        05 REQU-DAREGDAT-SPAR                                             
002400                             PIC 9(8).                                    
002500*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002600        05 REQU-TIKLOCK-SPAR PIC 9(8).                                    
002700*                                 KLOCKSLAG (TTMMSSTH)                    
002800     03 REQU-FLSNDDOK        PIC X.                                       
002900*                                 ALLMÄN FLAGGA                           
003000     03 REQU-KVRADER         PIC 9(5).                                    
003100*                                 ANTAL RADER                             
003200     03 REQU-INPUTLINE       OCCURS 500 TIMES.                            
003300*                                 INMATNINGS-/NYCKELFÄLT PÅ RADEN         
003400*                                                                         
003500        05 REQU-KDCMD        PIC X(4).                                    
003600        05 REQU-IDKOLLI      PIC 9(5).                                    
003700*                                 KOLLINUMMER                             
003800        05 REQU-IDRT         PIC X(3).                                    
003900*                                 RETURTERMINAL                           
004000        05 REQU-IDRTLOP      PIC 9(3).                                    
004100*                                 RETUR TERMINAL LÖPNUMMER                
004200        05 REQU-IDDC         PIC X(2).                                    
004300*                                 IDENTIFIERARE LAGER                     
004400*** END OF VILMAII-COPY LENGTH= 8546 BYTES                                
