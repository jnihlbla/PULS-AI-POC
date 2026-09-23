000100 01  WF10P003.                                                            
000200*                                 VALUTAKURSER FR≈N SAP R/3               
000300     03 IDPTYP               PIC X(4).                                    
000400*                                 POSTTYP              IDPTYP-004         
000500*                                 RECORD TYPE          IDPTYP-004         
000600     03 IDLEGSEL             PIC X(4).                                    
000700*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000800*                                 LEGAL SELLER IDENTITY                   
000900     03 KDVALISO             PIC X(5).                                    
001000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001100*                                 CURRENCY CODE BY ISO-STANDARD.          
001200     03 DASTADAT             PIC 9(8).                                    
001300*                                 GENERELLT STARTDATUM                    
001400*                                 GENERAL START DATE                      
001500     03 REVALUTA             PIC 9(5).                                    
001600*                                 OMRƒKNINGSTAL F÷R VALUTA                
001700*                                 CONVERT VALUE FOR CURRENCY CODE         
001800     03 PRKURS               PIC 9(6)V9(5).                               
001900*                                 VALUTAKURS                              
002000*                                 CURRENCY EXCHANGE RATE                  
002100     03 DAREGDAT             PIC 9(8).                                    
002200*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
002300*                                 REGISTRATION DATE (YYYYMMDD)            
002400*** END OF VILMAII-COPY LENGTH= 45 BYTES                                  
