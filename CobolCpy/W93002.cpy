000100 01  SAP-W93001AA.                                                        
000200*                                 VALUTAKURSER FR≈N SAP R/3               
000300     03 SAP-IDPTYP           PIC X(4).                                    
000400*                                 POSTTYP              IDPTYP-004         
000500*                                 RECORD TYPE          IDPTYP-004         
000600     03 SAP-KDVALISO         PIC X(3).                                    
000700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000800*                                 CURRENCY CODE BY ISO-STANDARD.          
000900     03 SAP-FILLER           PIC X(2).                                    
001000     03 SAP-REVALUTA         PIC 9(5).                                    
001100*                                 OMRƒKNINGSTAL F÷R VALUTA                
001200*                                 CONVERT VALUE FOR CURRENCY CODE         
001300     03 SAP-PRKURS           PIC 9(6)V9(5).                               
001400*                                 VALUTAKURS                              
001500*                                 CURRENCY EXCHANGE RATE                  
001600     03 SAP-DASTADAT         PIC 9(8).                                    
001700*                                 GENERELLT STARTDATUM                    
001800*                                 GENERAL START DATE                      
001900     03 SAP-DASEND           PIC 9(8).                                    
002000*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
002100*                                 REGISTRATION DATE (YYYYMMDD)            
002200*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
