000100 01  SYNQC-W488ORCN.                                                      
000200*                                 COPYBOOK FOR ORDER CANCEL               
000300*                                 SUB PROGRAM WHICH IS CALLED             
000400*                                 FROM 6164 4318 4325                     
000500     03 SYNQC-ORDERTYPE      PIC X(3).                                    
000600     03 SYNQC-IDDC           PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 SYNQC-IDARTNR        PIC 9(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 SYNQC-TIORDTIME      PIC 9(12).                                   
001100     03 SYNQC-TIRFSDAT       PIC 9(6).                                    
001200*                                 KLART FÖR TRANSPORT ÅÅMMDD              
001300     03 SYNQC-IDPRODNR       PIC 9(7).                                    
001400*                                 PRODUKTIONSNUMMER                       
001500     03 SYNQC-IDRADNR        PIC 9(4).                                    
001600*                                 RADNUMMER                               
001700     03 SYNQC-KDSVAR         PIC X.                                       
001800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001900     03 SYNQC-IDPLKLST       PIC 9(3).                                    
002000*                                 PLOCKLISTNUMMER                         
002100*** END OF VILMAII-COPY LENGTH= 47 BYTES                                  
