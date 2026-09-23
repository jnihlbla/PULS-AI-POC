000100 01  WF10P002.                                                            
000200*                                 VALUTAKURSER FRÅN SAP R/3               
000300     03 IDLEGSEL             PIC X(4).                                    
000400*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000500*                                 LEGAL SELLER IDENTITY                   
000600     03 KDVALISO             PIC X(5).                                    
000700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000800*                                 CURRENCY CODE BY ISO-STANDARD.          
000900     03 DASTADAT             PIC 9(8).                                    
001000*                                 GENERELLT STARTDATUM                    
001100*                                 GENERAL START DATE                      
001200     03 REVALUTA             PIC 9(5).                                    
001300*                                 OMRÄKNINGSTAL FÖR VALUTA                
001400*                                 CONVERT VALUE FOR CURRENCY CODE         
001500     03 PRKURS               PIC 9(6)V9(5).                               
001600*                                 VALUTAKURS                              
001700*                                 CURRENCY EXCHANGE RATE                  
001800     03 DAREGDAT             PIC 9(8).                                    
001900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002000*                                 REGISTRATION DATE (YYYYMMDD)            
002100     03 REVALUTA-FROM        PIC 9(5).                                    
002200*                                 OMRÄKNINGSTAL FÖR VALUTA                
002300*                                 CONVERT VALUE FOR CURRENCY CODE         
002400     03 REVALUTA-TO          PIC 9(5).                                    
002500*                                 OMRÄKNINGSTAL FÖR VALUTA                
002600*                                 CONVERT VALUE FOR CURRENCY CODE         
002700*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  
