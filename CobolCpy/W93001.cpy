000100 01  SAP-W93001.                                                          
000200*                                 VALUTAKURSER FRÅN SAP R/3               
000300     03 SAP-IDFTG            PIC 9(2).                                    
000400*                                 FÖRETAGSID EKONOM REDOVISNING           
000500*                                 COMPANY IDENTITY ACCOUNTING             
000600     03 SAP-KDVALISO         PIC X(3).                                    
000700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000800*                                 CURRENCY CODE BY ISO-STANDARD.          
000900     03 SAP-FILLERX          PIC X.                                       
001000     03 SAP-REVALUTA         PIC 9(3).                                    
001100*                                 OMRÄKNINGSTAL FÖR VALUTA                
001200*                                 CONVERT VALUE FOR CURRENCY CODE         
001300     03 SAP-PRKURS           PIC 9(6)V9(5).                               
001400*                                 VALUTAKURS                              
001500*                                 CURRENCY EXCHANGE RATE                  
001600     03 SAP-TISTADAT         PIC 9(6).                                    
001700*                                 GENERELLT STARTDATUM                    
001800*                                 GENERAL START DATE                      
001900     03 SAP-TISTODAT         PIC 9(6).                                    
002000*                                 GENERELLT STOPPDATUM                    
002100*                                 GENERAL STOP DATE YYMMDD                
002200*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
