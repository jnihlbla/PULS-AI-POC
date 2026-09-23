000100 01  WF10CURR.                                                            
000200*                                 VALUTAKURSER FRÅN SAP R/3               
000300     03 KDVALISO             PIC X(5).                                    
000400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000500*                                 CURRENCY CODE BY ISO-STANDARD.          
000600     03 REVALUTA-FROM        PIC 9(5).                                    
000700*                                 OMRÄKNINGSTAL FÖR VALUTA                
000800*                                 CONVERT VALUE FOR CURRENCY CODE         
000900     03 REVALUTA-TO          PIC 9(5).                                    
001000*                                 OMRÄKNINGSTAL FÖR VALUTA                
001100*                                 CONVERT VALUE FOR CURRENCY CODE         
001200     03 PRKURS               PIC 9(6)V9(5).                               
001300*                                 VALUTAKURS                              
001400*                                 CURRENCY EXCHANGE RATE                  
001500     03 DASTADAT             PIC 9(8).                                    
001600*                                 GENERELLT STARTDATUM                    
001700*                                 GENERAL START DATE                      
001800     03 DAREGDAT             PIC 9(8).                                    
001900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002000*                                 REGISTRATION DATE (YYYYMMDD)            
002100     03 IDLEGSEL             PIC X(4).                                    
002200*                                 FAKTURERANDE FÖRETAG TEX VCCS           
002300*                                 LEGAL SELLER IDENTITY                   
002400*** END OF VILMAII-COPY LENGTH= 46 BYTES                                  
