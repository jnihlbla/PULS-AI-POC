000100 01  W61247.                                                              
000200*                                 LDC REFILL AVVIKELSE VID INLEVE         
000300*                                 RANS                                    
000400     03 DAFAKT               PIC 9(8).                                    
000500*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDDC-REC             PIC X(2).                                    
000900*                                 MOTTAGANDE LAGER                        
001000     03 IDDC-SEND            PIC X(2).                                    
001100*                                 SÄNDANDE LAGER                          
001200     03 IDFAKT               PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600     03 IDKUNDRF             PIC X(10).                                   
001700*                                 KUNDENS REFERENS (ORDERID)              
001800     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001900*                                 KOLLINUMMER                             
002000     03 KVAVIS               PIC S9(7)           COMP-3.                  
002100*                                 AVISERAT ANTAL                          
002200     03 KVANTAL              PIC S9(7)           COMP-3.                  
002300*                                 ANTAL                                   
002400     03 AVVIKELSETYP         PIC X(10).                                   
002500     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
002600*                                 ARTIKELSTANDARDPRIS                     
002700     03 KDSORT1              PIC S9              COMP-3.                  
002800*                                 SORTERINGSKOD                           
002900     03 FLINLREP             PIC X.                                       
003000*                                 FLAGGA AVVIKELSERAPPORTER               
003100     03 DAREGDAT             PIC 9(8).                                    
003200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
003300     03 IDUSER               PIC X(8).                                    
003400*                                 ANVÄNDARENS SÄKERHETS ID                
003500*** END OF VILMAII-COPY LENGTH= 79 BYTES                                  
