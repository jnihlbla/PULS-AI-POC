000100 01  RYA-WDGZRYA.                                                         
000200*                                 RYA                                     
000300*                                 SKAPAS FÖR TPO-ORDERRADER VID           
000400*                                 ORDERENTRY, FÖRÄNDRING OCH              
000500*                                 ANNULLATION.                            
000600*                                 ANVÄNDS VID TRANSAKTION-                
000700*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000800     03 RYA-IDPTYP           PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 RYA-IDDISTR          PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200     03 RYA-IDKUNDNR         PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400     03 RYA-IDKUNDRF         PIC X(10).                                   
001500*                                 KUNDENS REFERENS (ORDERID)              
001600     03 RYA-IDARTNR          PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 RYA-REKSIFFR         PIC S9              COMP-3.                  
001900*                                 KONTROLLSIFFRA                          
002000     03 RYA-KVBEART          PIC S9(7)           COMP-3.                  
002100*                                 BESTÄLLT ANTAL STYCKEN                  
002200     03 RYA-TITPO            PIC S9(7)           COMP-3.                  
002300*                                 PLANERAD ORDERDATUM                     
002400     03 RYA-KDTPOTYP         PIC S9              COMP-3.                  
002500*                                 TYP AV TIDPLANERAD ORDER                
002600     03 RYA-KDVRTPO          PIC S9              COMP-3.                  
002700*                                 KOD FÖR TPO:ER FRÅN VR                  
002800     03 RYA-KDVRINFO         PIC S9              COMP-3.                  
002900*                                 PÅVERKAN I VR/DSP SYSTEM                
003000*** END COPY WDGZRYA     LENGTH=37                                        
