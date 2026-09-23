000100 01  RET-WDL223.                                                          
000200*                                 INLEVERANS HISTORIK                     
000300*                                 R40 RETURER                             
000400*                                 FYSISK NYCKEL SAKNAS                    
000500*                                 SÖKBEGREPP IDPTYP                       
000600*                                            IDLOPNRM                     
000700     03 RET-IDPTYP           PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 RET-IDLOPNRM         PIC S9(9)           COMP-3.                  
001000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001100*                                 (0VVDLLLLK)                             
001200     03 RET-IDLEVNR          PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 RET-IDORDNR          PIC S9(5)           COMP-3.                  
001500*                                 ORDERNUMMER UTGÅR PD90                  
001600     03 RET-IDDC             PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 RET-KVRETUR          PIC S9(7)           COMP-3.                  
001900*                                 ANTAL I RETUR                           
002000     03 RET-FILLER           PIC X(3).                                    
002100*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
