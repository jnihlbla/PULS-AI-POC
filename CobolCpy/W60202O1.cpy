000100 01  RESP-W60202O1.                                                       
000200*                                 RESP-COPYTEXT                           
000300*                                 FÖR W60202                              
000400     03 RESP-IDKR-KEY        PIC Z(4)9.                                   
000500*                                 KONTROLLRAPPORT NUMMER                  
000600*                                 INSPECTION REPORT NUMBER                
000700     03 RESP-IDKR-INFO-MSG   PIC Z(4)9.                                   
000800*                                 KONTROLLRAPPORT NUMMER                  
000900*                                 INSPECTION REPORT NUMBER                
001000     03 RESP-IDLOPNRM-UPD-ATTR                                            
001100                             PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 RESP-IDLOPNRM-UPD    PIC X(8).                                    
001400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001500*                                 (0VVDLLLLK)                             
001600*                                 SERIAL NO RECEIVING REPORT              
001700*                                 (0WWDLLLLC)                             
001800     03 RESP-IDLOPNRM-UT     PIC Z(7)9.                                   
001900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002000*                                 (0VVDLLLLK)                             
002100*                                 SERIAL NO RECEIVING REPORT              
002200*                                 (0WWDLLLLC)                             
002300     03 RESP-IDAVINR         PIC Z(6)9.                                   
002400*                                 AVI-NUMMER                              
002500*                                 ADVICE NOTE NUMBER                      
002600     03 RESP-KVAVIS          PIC Z(5)9.                                   
002700*                                 AVISERAT ANTAL                          
002800*                                 QUANTITY NOTIFIED                       
002900     03 RESP-TIAVSDAT-UPD    PIC X(6).                                    
003000*                                 AVISERINGSDATUM (YYMMDD)                
003100*                                 ADVICE NOTE DATE                        
003200     03 RESP-IDARTNR-UPD-ATTR                                             
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 RESP-IDARTNR-UPD     PIC X(9).                                    
003600*                                 ARTIKELNUMMER                           
003700*                                 PART NUMBER                             
003800     03 RESP-IDARTNR-UT      PIC Z(9).                                    
003900*                                 ARTIKELNUMMER                           
004000*                                 PART NUMBER                             
004100     03 RESP-BEART           PIC X(25).                                   
004200*                                 ARTIKELBENÄMNING                        
004300*                                 PART DESCRIPTION                        
004400     03 RESP-TIREGDAT        PIC 9(6).                                    
004500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004600*                                 REGISTRATION DATE (YYMMDD)              
004700     03 RESP-IDLEVNR-UPD-ATTR                                             
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 RESP-IDLEVNR-UPD     PIC X(5).                                    
005100*                                 LEVERANTÖRNUMMER                        
005200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005300     03 RESP-IDLEVNR-UT      PIC X(5).                                    
005400*                                 LEVERANTÖRNUMMER                        
005500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005600     03 RESP-IDLEVG-UPD-ATTR PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 RESP-IDLEVG-UPD      PIC X(5).                                    
005900*                                 LEVERANTÖRS GODSADRESS NUMMER           
006000*                                 SUPPLIER WAREHOUSE NUMBER               
006100     03 RESP-IDLEVG-UT       PIC Z(4)9.                                   
006200*                                 LEVERANTÖRS GODSADRESS NUMMER           
006300*                                 SUPPLIER WAREHOUSE NUMBER               
006400     03 RESP-BELEV           PIC X(30).                                   
006500     03 RESP-KDKRSTA         PIC X.                                       
006600*                                 KONTROLLRAPPORT STATUS                  
006700*                                 INSPECTION REPORT STATUS                
006800     03 RESP-KVANTMOT-UPD-ATTR                                            
006900                             PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 RESP-KVANTMOT-UPD    PIC X(6).                                    
007200*                                 ANTAL MOTTAGET                          
007300*                                 QUANTITY RECEIVED                       
007400     03 RESP-KVANTMOT-UT     PIC Z(5)9.                                   
007500*                                 ANTAL MOTTAGET                          
007600*                                 QUANTITY RECEIVED                       
007700     03 RESP-KVART-RET-UPD-ATTR                                           
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 RESP-KVART-RET-UPD   PIC X(7).                                    
008100*                                 ANTAL ARTIKLAR I RETUR                  
008200*                                 QUANTITY INSPECTED PARTS                
008300     03 RESP-KVART-RET-UT    PIC Z(6)9.                                   
008400*                                 ANTAL ARTIKLAR I RETUR                  
008500*                                 QUANTITY INSPECTED PARTS                
008600     03 RESP-KVART-AAVV      PIC -(6)9.                                   
008700*                                 ANTALSAVVIKELSE FÖR ARTIKEL             
008800*                                 QUANTITY INSPECTED PARTS                
008900     03 RESP-KVART-SKROT-UPD-ATTR                                         
009000                             PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200     03 RESP-KVART-SKROT-UPD PIC X(7).                                    
009300*                                 ANTAL SKROTADE ARTIKLAR                 
009400*                                 QUANTITY INSPECTED PARTS                
009500     03 RESP-KVART-SKROT-UT  PIC Z(6)9.                                   
009600*                                 ANTAL SKROTADE ARTIKLAR                 
009700*                                 QUANTITY INSPECTED PARTS                
009800     03 RESP-KVART-SKROT-LDC-UPD-ATTR                                     
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 RESP-KVART-SKROT-LDC-UPD                                          
010200                             PIC Z(6)9.                                   
010300*                                 ANTAL SKROTADE ARTIKLAR LDC             
010400*                                 QUANTITY INSPECTED PARTS LDC            
010500     03 RESP-KVART-SKROT-LDC-UT                                           
010600                             PIC Z(6)9.                                   
010700*                                 ANTAL SKROTADE ARTIKLAR LDC             
010800*                                 QUANTITY INSPECTED PARTS LDC            
010900     03 RESP-KVART-KJUST-UPD-ATTR                                         
011000                             PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 RESP-KVART-KJUST-UPD PIC X(7).                                    
011300*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
011400*                                 QUANTITY INSPECTED PARTS                
011500     03 RESP-KVART-KJUST-UT  PIC Z(6)9.                                   
011600*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
011700*                                 QUANTITY INSPECTED PARTS                
011800     03 RESP-KVART-KONTR-UPD-ATTR                                         
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 RESP-KVART-KONTR-UPD PIC X(7).                                    
012200*                                 ANTAL KONTROLLERAD ARTIKLAR             
012300*                                 QUANTITY INSPECTED PARTS                
012400     03 RESP-KVART-KONTR-UT  PIC Z(6)9.                                   
012500*                                 ANTAL KONTROLLERAD ARTIKLAR             
012600*                                 QUANTITY INSPECTED PARTS                
012700     03 RESP-KVART-BEH-UPD-ATTR                                           
012800                             PIC X(2).                                    
012900*                                 MFS ATTRIBUTFÄLT                        
013000     03 RESP-KVART-BEH-UPD   PIC X(7).                                    
013100*                                 ANTAL ARTIKLAR SOM BEHÅLLES             
013200*                                 QUANTITY INSPECTED PARTS                
013300     03 RESP-KVART-BEH-UT    PIC Z(6)9.                                   
013400*                                 ANTAL ARTIKLAR SOM BEHÅLLES             
013500*                                 QUANTITY INSPECTED PARTS                
013600     03 RESP-KVART-EJ-GODK-UPD-ATTR                                       
013700                             PIC X(2).                                    
013800*                                 MFS ATTRIBUTFÄLT                        
013900     03 RESP-KVART-EJ-GODK-UPD                                            
014000                             PIC X(7).                                    
014100*                                 ANTAL EJ GODKÄNDA ARTIKLAR              
014200*                                 QUANTITY INSPECTED PARTS                
014300     03 RESP-KVART-EJ-GODK-UT                                             
014400                             PIC Z(6)9.                                   
014500*                                 ANTAL EJ GODKÄNDA ARTIKLAR              
014600*                                 QUANTITY INSPECTED PARTS                
014700     03 RESP-KVART-SJUST-UPD-ATTR                                         
014800                             PIC X(2).                                    
014900*                                 MFS ATTRIBUTFÄLT                        
015000     03 RESP-KVART-SJUST-UPD PIC X(7).                                    
015100*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
015200*                                 QUANTITY INSPECTED PARTS                
015300     03 RESP-KVART-SJUST-UT  PIC -(6)9.                                   
015400*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
015500*                                 QUANTITY INSPECTED PARTS                
015600     03 RESP-LINE.                                                        
015700        05 RESP-IDKRFEL-UPD-ATTR                                          
015800                             PIC X(2).                                    
015900*                                 MFS ATTRIBUTFÄLT                        
016000        05 RESP-IDKRFEL-UPD  PIC X(2).                                    
016100*                                 FELKOD FÖR KONTROLLRAPPORT              
016200*                                 ERRORCODE FOR INSP.REPORT               
016300        05 RESP-BEKRFEL      PIC X(60).                                   
016400*                                 BESKRIVNING FELKOD KONTR.RAPPOR         
016500*                                 T                                       
016600*                                 DESCRIPTION OF ERRORCODE INSP.R         
016700*                                 EPORT                                   
016800     03 RESP-FLKVALSP-UPD-ATTR                                            
016900                             PIC X(2).                                    
017000*                                 MFS ATTRIBUTFÄLT                        
017100     03 RESP-FLKVALSP-UPD    PIC X.                                       
017200*                                 KVALITETSBLOCK JUSTERAS                 
017300*                                 QUANLITY BLOCK ADJUSTMENT               
017400     03 RESP-FLKVALSP-UT     PIC X.                                       
017500*                                 KVALITETSBLOCK JUSTERAS                 
017600*                                 QUANLITY BLOCK ADJUSTMENT               
017700     03 RESP-KDDISP-UPD-ATTR PIC X(2).                                    
017800*                                 MFS ATTRIBUTFÄLT                        
017900     03 RESP-KDDISP-UPD      PIC 9(2).                                    
018000*                                 DISPOSITION CODE                        
018100*                                 DISPOSITION CODE                        
018200     03 RESP-FLBUFJUS-UPD-ATTR                                            
018300                             PIC X(2).                                    
018400*                                 MFS ATTRIBUTFÄLT                        
018500     03 RESP-FLBUFJUS-UPD    PIC X.                                       
018600*                                 BUFFERTJUSERING                         
018700*                                 ADJUST BUFFER                           
018800     03 RESP-FLBUFJUS-UT     PIC X.                                       
018900*                                 BUFFERTJUSERING                         
019000*                                 ADJUST BUFFER                           
019100     03 RESP-TEDISP          PIC X(66).                                   
019200*                                 TEXTFÄLT                                
019300*                                 TEXT FIELD                              
019400     03 RESP-KDHANDCO-UPD-ATTR                                            
019500                             PIC X(2).                                    
019600*                                 MFS ATTRIBUTFÄLT                        
019700     03 RESP-KDHANDCO-UPD    PIC 9.                                       
019800*                                 OMKOSTNADSKOD                           
019900*                                 HANDLING COST CODE                      
020000     03 RESP-TEHANDCO        PIC X(51).                                   
020100     03 RESP-FLAGGA-FELTEXT  PIC X.                                       
020200*                                 ALLMÄN FLAGGA                           
020300*                                 GENERAL FLAG                            
020400     03 RESP-KDPERSON-UPD-ATTR                                            
020500                             PIC X(2).                                    
020600*                                 MFS ATTRIBUTFÄLT                        
020700     03 RESP-KDPERSON-UPD    PIC Z(2)9.                                   
020800*                                 PERSONKOD                               
020900*                                 STAFF CODE                              
021000     03 RESP-BEKRBEH-UPD-ATTR                                             
021100                             PIC X(2).                                    
021200*                                 MFS ATTRIBUTFÄLT                        
021300     03 RESP-BEKRBEH-UPD     PIC X(25).                                   
021400*                                 KONTROLLANT                             
021500*                                 INSPECTOR                               
021600     03 RESP-KVKRBEH-UPD-ATTR                                             
021700                             PIC X(2).                                    
021800*                                 MFS ATTRIBUTFÄLT                        
021900     03 RESP-KVKRBEH-UPD     PIC X(4).                                    
022000*                                 BEHANDLINGSTID FÖR KR                   
022100*                                 USED TIME FOR INSP. REPORT              
022200     03 RESP-KVKRBEH-UT      PIC Z9.9.                                    
022300*                                 BEHANDLINGSTID FÖR KR                   
022400*                                 USED TIME FOR INSP. REPORT              
022500     03 RESP-TEKRPLT-UPD-ATTR                                             
022600                             PIC X(2).                                    
022700*                                 MFS ATTRIBUTFÄLT                        
022800     03 RESP-TEKRPLT-UPD     PIC X(20).                                   
022900*                                 GODS PLACERAT                           
023000*                                 GOODS PLACED                            
023100*** END OF VILMAII-COPY LENGTH= 563 BYTES                                 
