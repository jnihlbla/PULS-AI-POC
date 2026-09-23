000100 01  REQU-WL0137I1.                                                       
000200*                                 COYPTEXT FOR PGM WL0137                 
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 REQU-IDARTNR-KEY     PIC 9(8).                                    
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 REQU-IDKVAINF-KEY    PIC 9(2).                                    
001000*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
001100*                                 LINENO FOR QUALITY CONTROL TEXT         
001200     03 REQU-TIREGDAT-KEY    PIC 9(6).                                    
001300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001400*                                 REGISTRATION DATE (YYMMDD)              
001500     03 REQU-IDDC2-KEY       PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 REQU-SHOW-ALL-KEY    PIC X.                                       
001900*                                 ALLMÄN FLAGGA                           
002000*                                 GENERAL FLAG                            
002100     03 REQU-TIREGDAT-NEXT   PIC 9(7).                                    
002200*                                 DATUMETS 9-KOMPLEMENT                   
002300*                                 DATES 9-COMPLEMENT                      
002400     03 REQU-TIKLOCK-NEXT    PIC 9(9).                                    
002500*                                 TID LAGRAT SOM 9-KOMPLEMENT             
002600*                                 TIME SAVED AS 9-COMPLEMENT              
002700     03 REQU-KVRADER         PIC 9(5).                                    
002800*                                 ANTAL RADER                             
002900*                                 NUMBER OF LINES                         
003000     03 REQU-IDDC-IN         PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200*                                 WAREHOUSE IDENTIFIER                    
003300     03 REQU-TISTADAT-IN     PIC 9(6).                                    
003400*                                 GENERELLT STARTDATUM                    
003500*                                 GENERAL START DATE                      
003600     03 REQU-TISTODAT-IN     PIC 9(6).                                    
003700*                                 GENERELLT STOPPDATUM                    
003800*                                 GENERAL STOP DATE YYMMDD                
003900     03 REQU-KVANTAL-IN      PIC 9(7).                                    
004000*                                 ANTAL                                   
004100*                                 NUMBER                                  
004200     03 REQU-KVAVV-KVAL-IN   PIC 9(7).                                    
004300*                                 ANTALSAVVIKELSE KVALITET                
004400*                                 QUANTITYDEVIATION QUALITY               
004500     03 REQU-KVART-SKROT-IN  PIC 9(7).                                    
004600*                                 ANTAL SKROTADE ARTIKLAR                 
004700*                                 QUANTITY INSPECTED PARTS                
004800     03 REQU-KVART-RET-IN    PIC 9(7).                                    
004900*                                 ANTAL ARTIKLAR I RETUR                  
005000*                                 QUANTITY INSPECTED PARTS                
005100     03 REQU-KVART-KJUST-IN  PIC 9(7).                                    
005200*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
005300*                                 QUANTITY INSPECTED PARTS                
005400     03 REQU-BEINIT-IN       PIC X(3).                                    
005500*                                 INITIALER FÖR EN PERSON                 
005600*                                 INITIALS FOR A PERSON                   
005700*** END OF VILMAII-COPY LENGTH= 94 BYTES                                  
