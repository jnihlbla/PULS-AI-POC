000100 01  W61188.                                                              
000200*                                 INFORMATION OM                          
000300*                                 RENSADE INLEVERANSER PÅ                 
000400*                                 HISTORIK-REGISTRET                      
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700*                                 RECORD TYPE                             
000800     03 IDARTNR              PIC 9(8).                                    
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 TIAAVVD              PIC 9(5).                                    
001200*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
001300*                                 YEAR - WEEK - DAY  (YYWWD)              
001400     03 IDLOPNRM             PIC 9(8).                                    
001500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001600*                                 (0VVDLLLLK)                             
001700*                                 SERIAL NO RECEIVING REPORT              
001800*                                 (0WWDLLLLC)                             
001900     03 IDAVINR              PIC 9(7).                                    
002000*                                 AVI-NUMMER                              
002100*                                 ADVICE NOTE NUMBER                      
002200     03 IDKONTO              PIC 9(10).                                   
002300*                                 KONTO                                   
002400*                                 ACCOUNT                                 
002500     03 IDLEVNR              PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002800     03 IDORDNR              PIC 9(5).                                    
002900*                                 ORDERNUMMER UTGÅR PD90                  
003000*                                 ORDER NUMBER                            
003100     03 ADLAGOMR             PIC 9(2).                                    
003200*                                 LAGEROMRÅDE                             
003300*                                 AREA                                    
003400     03 ADGANG               PIC 9(2).                                    
003500*                                 GÅNG                                    
003600*                                 AISLE                                   
003700     03 ADPLATS              PIC 9(5).                                    
003800*                                 LAGERPLATSNUMMER                        
003900*                                 LOCATION                                
004000     03 IDDC                 PIC X(2).                                    
004100*                                 IDENTIFIERARE LAGER                     
004200*                                 WAREHOUSE IDENTIFIER                    
004300     03 KDRT                 PIC 9(2).                                    
004400*                                 REDOVISNINGSTYP                         
004500*                                 TYPE OF ACCOUNTING                      
004600     03 KDAVVANT             PIC 9.                                       
004700*                                 AVVIKELSEANTAL KOD                      
004800*                                 0=INGEN ANM.   1=AVVIKELSE              
004900*                                 2=MAKULERING AV MOTT.RAPPORT            
005000*                                 QUANTITY DEVIATION  CODE                
005100*                                 0=NO DEV.    1=DEVIATION                
005200*                                 2=CANCELLING OF REC. REPORT             
005300     03 KDAVVKV              PIC 9.                                       
005400*                                 KVALITETSAVVIKELSEKOD                   
005500*                                 0=INGEN ANM.  1=AVVIKELSE               
005600*                                 2=AVVIKELSE, RETURNERAS                 
005700*                                 QUALITY DEVIATION CODE                  
005800*                                 0 = NO DEV.  1 = DEVIATION              
005900*                                 2 = DEVIATION, WILL BE RETURNED         
006000     03 KVANTMOT             PIC S9(7).                                   
006100*                                 ANTAL MOTTAGET                          
006200*                                 QUANTITY RECEIVED                       
006300     03 KVAVIS               PIC 9(6).                                    
006400*                                 AVISERAT ANTAL                          
006500*                                 QUANTITY NOTIFIED                       
006600     03 KVFORDEL             PIC 9(6).                                    
006700*                                 ANTAL FÖRDELAT                          
006800*                                 SPLIT QUANTITY                          
006900     03 KVRETUR              PIC 9(7).                                    
007000*                                 ANTAL I RETUR                           
007100*                                 QUANTITY IN RETURN                      
007200     03 KVFORV               PIC 9(7).                                    
007300*                                 FÖRVÄNTAT ANTAL EFTER JUSTERING         
007400*                                 ADJUSTED QUANTITY                       
007500     03 TIAVSDAT             PIC 9(6).                                    
007600*                                 AVISERINGSDATUM (YYMMDD)                
007700*                                 ADVICE NOTE DATE                        
007800     03 TIUPPDAT             PIC 9(6).                                    
007900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
008000*                                 UPDATING DATE     (YYMMDD)              
008100     03 IDDISTR              PIC 9(4).                                    
008200*                                 DISTRIKTNUMMER                          
008300*                                 DISTRICT NUMBER                         
008400     03 IDKUNDNR             PIC 9(6).                                    
008500*                                 KUNDNUMMER                              
008600*                                 CUSTOMER NO                             
008700     03 IDFAKT               PIC 9(7).                                    
008800*                                 FAKTURANUMMER                           
008900*                                 INVOICE NO.                             
009000     03 IDKUNDRF             PIC X(10).                                   
009100*                                 KUNDENS REFERENS (ORDERID)              
009200*                                 CUSTOMER REFERENCE (ORDER ID)           
009300     03 IDPRODNR             PIC 9(7).                                    
009400*                                 PRODUKTIONSNUMMER                       
009500*                                 PRODUCTION NUMBER                       
009600     03 DAINLEV              PIC 9(16).                                   
009700*                                 INLEVERANS NUMMER                       
009800*                                 CONSIGNMENT IDENTITY                    
009900*                                 (YYYYMMDD+HHMMSSTH)                     
010000*** END OF VILMAII-COPY LENGTH= 161 BYTES                                 
