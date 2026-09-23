000100 01  MOD-W6O20201.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W60202                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDKR-IN          PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200*                                 MFS DISPOSITION OF INPUT FIELD          
001300     03 MOD-IDKR-UT          PIC 9(5).                                    
001400*                                 KONTROLLRAPPORT NUMMER                  
001500*                                 INSPECTION REPORT NUMBER                
001600     03 MOD-IDLOPNRM-ATTR    PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-IDLOPNRM-IN      PIC X(8).                                    
001900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002000*                                 (0VVDLLLLK)                             
002100*                                 SERIAL NO RECEIVING REPORT              
002200*                                 (0WWDLLLLC)                             
002300     03 MOD-IDLOPNRM-UT      PIC Z(7)9.                                   
002400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002500*                                 (0VVDLLLLK)                             
002600*                                 SERIAL NO RECEIVING REPORT              
002700*                                 (0WWDLLLLC)                             
002800     03 MOD-IDAVINR          PIC Z(6)9.                                   
002900*                                 AVI-NUMMER                              
003000*                                 ADVICE NOTE NUMBER                      
003100     03 MOD-KVAVIS           PIC Z(5)9.                                   
003200*                                 AVISERAT ANTAL                          
003300*                                 QUANTITY NOTIFIED                       
003400     03 MOD-TIAVSDAT         PIC X(6).                                    
003500*                                 AVISERINGSDATUM (YYMMDD)                
003600*                                 ADVICE NOTE DATE                        
003700     03 MOD-IDARTNR-ATTR     PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-IDARTNR-IN       PIC X(9).                                    
004000*                                 ARTIKELNUMMER                           
004100*                                 PART NUMBER                             
004200     03 MOD-IDARTNR-UT       PIC Z(9).                                    
004300*                                 ARTIKELNUMMER                           
004400*                                 PART NUMBER                             
004500     03 MOD-BEART            PIC X(25).                                   
004600*                                 ARTIKELBENÄMNING                        
004700*                                 PART DESCRIPTION                        
004800     03 MOD-TIREGDAT         PIC 9(6).                                    
004900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005000*                                 REGISTRATION DATE (YYMMDD)              
005100     03 MOD-IDLEVNR-ATTR     PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-IDLEVNR-IN       PIC X(5).                                    
005400*                                 LEVERANTÖRNUMMER                        
005500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005600     03 MOD-IDLEVNR-UT       PIC X(5).                                    
005700*                                 LEVERANTÖRNUMMER                        
005800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005900     03 MOD-IDLEVG-ATTR      PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-IDLEVG-IN        PIC X(5).                                    
006200*                                 LEVERANTÖRS GODSADRESS NUMMER           
006300*                                 SUPPLIER WAREHOUSE NUMBER               
006400     03 MOD-IDLEVG-UT        PIC Z(4)9.                                   
006500*                                 LEVERANTÖRS GODSADRESS NUMMER           
006600*                                 SUPPLIER WAREHOUSE NUMBER               
006700     03 MOD-BELEV            PIC X(30).                                   
006800     03 MOD-KDKRSTA          PIC X.                                       
006900*                                 KONTROLLRAPPORT STATUS                  
007000*                                 INSPECTION REPORT STATUS                
007100     03 MOD-KVANTMOT-ATTR    PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-KVANTMOT-IN      PIC X(6).                                    
007400*                                 ANTAL MOTTAGET                          
007500*                                 QUANTITY RECEIVED                       
007600     03 MOD-KVANTMOT-UT      PIC Z(5)9.                                   
007700*                                 ANTAL MOTTAGET                          
007800*                                 QUANTITY RECEIVED                       
007900     03 MOD-KVART-RET-ATTR   PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 MOD-KVART-RET-IN     PIC X(7).                                    
008200*                                 ANTAL ARTIKLAR I RETUR                  
008300*                                 QUANTITY INSPECTED PARTS                
008400     03 MOD-KVART-RET-UT     PIC Z(6)9.                                   
008500*                                 ANTAL ARTIKLAR I RETUR                  
008600*                                 QUANTITY INSPECTED PARTS                
008700     03 MOD-KVART-AAVV-UT    PIC -(6)9.                                   
008800*                                 ANTALSAVVIKELSE FÖR ARTIKEL             
008900*                                 QUANTITY INSPECTED PARTS                
009000     03 MOD-KVART-SKROT-ATTR PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200     03 MOD-KVART-SKROT-IN   PIC X(7).                                    
009300*                                 ANTAL SKROTADE ARTIKLAR                 
009400*                                 QUANTITY INSPECTED PARTS                
009500     03 MOD-KVART-SKROT-UT   PIC Z(6)9.                                   
009600*                                 ANTAL SKROTADE ARTIKLAR                 
009700*                                 QUANTITY INSPECTED PARTS                
009800     03 MOD-KVART-SKROT-LDC-ATTR                                          
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 MOD-KVART-SKROT-LDC-IN                                            
010200                             PIC Z(6)9.                                   
010300*                                 ANTAL SKROTADE ARTIKLAR LDC             
010400*                                 QUANTITY INSPECTED PARTS LDC            
010500     03 MOD-KVART-SKROT-LDC-UT                                            
010600                             PIC Z(6)9.                                   
010700*                                 ANTAL SKROTADE ARTIKLAR LDC             
010800*                                 QUANTITY INSPECTED PARTS LDC            
010900     03 MOD-KVART-KJUST-ATTR PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100     03 MOD-KVART-KJUST-IN   PIC X(7).                                    
011200*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
011300*                                 QUANTITY INSPECTED PARTS                
011400     03 MOD-KVART-KJUST-UT   PIC Z(6)9.                                   
011500*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
011600*                                 QUANTITY INSPECTED PARTS                
011700     03 MOD-KVART-KONTR-ATTR PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900     03 MOD-KVART-KONTR-IN   PIC X(7).                                    
012000*                                 ANTAL KONTROLLERAD ARTIKLAR             
012100*                                 QUANTITY INSPECTED PARTS                
012200     03 MOD-KVART-KONTR-UT   PIC Z(6)9.                                   
012300*                                 ANTAL KONTROLLERAD ARTIKLAR             
012400*                                 QUANTITY INSPECTED PARTS                
012500     03 MOD-KVART-BEH-ATTR   PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700     03 MOD-KVART-BEH-IN     PIC X(7).                                    
012800*                                 ANTAL ARTIKLAR SOM BEHÅLLES             
012900*                                 QUANTITY INSPECTED PARTS                
013000     03 MOD-KVART-BEH-UT     PIC Z(6)9.                                   
013100*                                 ANTAL ARTIKLAR SOM BEHÅLLES             
013200*                                 QUANTITY INSPECTED PARTS                
013300     03 MOD-KVART-EJ-GODK-ATTR                                            
013400                             PIC X(2).                                    
013500*                                 MFS ATTRIBUTFÄLT                        
013600     03 MOD-KVART-EJ-GODK-IN PIC X(7).                                    
013700*                                 ANTAL EJ GODKÄNDA ARTIKLAR              
013800*                                 QUANTITY INSPECTED PARTS                
013900     03 MOD-KVART-EJ-GODK-UT PIC Z(6)9.                                   
014000*                                 ANTAL EJ GODKÄNDA ARTIKLAR              
014100*                                 QUANTITY INSPECTED PARTS                
014200     03 MOD-KVART-SJUST-ATTR PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400     03 MOD-KVART-SJUST-IN   PIC X(7).                                    
014500*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
014600*                                 QUANTITY INSPECTED PARTS                
014700     03 MOD-KVART-SJUST-UT   PIC -(6)9.                                   
014800*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
014900*                                 QUANTITY INSPECTED PARTS                
015000     03 MOD-RAD.                                                          
015100        05 MOD-IDKRFEL-ATTR  PIC X(2).                                    
015200*                                 MFS ATTRIBUTFÄLT                        
015300        05 MOD-IDKRFEL       PIC X(2).                                    
015400*                                 FELKOD FÖR KONTROLLRAPPORT              
015500*                                 ERRORCODE FOR INSP.REPORT               
015600        05 MOD-BEKRFEL       PIC X(60).                                   
015700*                                 BESKRIVNING FELKOD KONTR.RAPPOR         
015800*                                 T                                       
015900*                                 DESCRIPTION OF ERRORCODE INSP.R         
016000*                                 EPORT                                   
016100     03 MOD-FLKVALSP-ATTR    PIC X(2).                                    
016200*                                 MFS ATTRIBUTFÄLT                        
016300     03 MOD-FLKVALSP-IN      PIC X.                                       
016400*                                 KVALITETSBLOCK JUSTERAS                 
016500*                                 QUANLITY BLOCK ADJUSTMENT               
016600     03 MOD-FLKVALSP-UT      PIC X.                                       
016700*                                 KVALITETSBLOCK JUSTERAS                 
016800*                                 QUANLITY BLOCK ADJUSTMENT               
016900     03 MOD-KDDISP-ATTR      PIC X(2).                                    
017000*                                 MFS ATTRIBUTFÄLT                        
017100     03 MOD-KDDISP           PIC 9(2).                                    
017200*                                 DISPOSITION CODE                        
017300*                                 DISPOSITION CODE                        
017400     03 MOD-FLBUFJUS-ATTR    PIC X(2).                                    
017500*                                 MFS ATTRIBUTFÄLT                        
017600     03 MOD-FLBUFJUS-IN      PIC X.                                       
017700*                                 BUFFERTJUSERING                         
017800*                                 ADJUST BUFFER                           
017900     03 MOD-FLBUFJUS-UT      PIC X.                                       
018000*                                 BUFFERTJUSERING                         
018100*                                 ADJUST BUFFER                           
018200     03 MOD-TEDISP           PIC X(66).                                   
018300*                                 TEXTFÄLT                                
018400*                                 TEXT FIELD                              
018500     03 MOD-KDHANDCO-ATTR    PIC X(2).                                    
018600*                                 MFS ATTRIBUTFÄLT                        
018700     03 MOD-KDHANDCO         PIC 9.                                       
018800*                                 OMKOSTNADSKOD                           
018900*                                 HANDLING COST CODE                      
019000     03 MOD-TEHANDCO         PIC X(51).                                   
019100     03 MOD-FLAGGA-FELTEXT   PIC X.                                       
019200*                                 ALLMÄN FLAGGA                           
019300*                                 GENERAL FLAG                            
019400     03 MOD-KDPERSON-ATTR    PIC X(2).                                    
019500*                                 MFS ATTRIBUTFÄLT                        
019600     03 MOD-KDPERSON         PIC Z(2)9.                                   
019700*                                 PERSONKOD                               
019800*                                 STAFF CODE                              
019900     03 MOD-BEKRBEH-ATTR     PIC X(2).                                    
020000*                                 MFS ATTRIBUTFÄLT                        
020100     03 MOD-BEKRBEH          PIC X(25).                                   
020200*                                 KONTROLLANT                             
020300*                                 INSPECTOR                               
020400     03 MOD-KVKRBEH-ATTR     PIC X(2).                                    
020500*                                 MFS ATTRIBUTFÄLT                        
020600     03 MOD-KVKRBEH-IN       PIC X(4).                                    
020700*                                 BEHANDLINGSTID FÖR KR                   
020800*                                 USED TIME FOR INSP. REPORT              
020900     03 MOD-KVKRBEH-UT       PIC Z9.9.                                    
021000*                                 BEHANDLINGSTID FÖR KR                   
021100*                                 USED TIME FOR INSP. REPORT              
021200     03 MOD-TEKRPLT-ATTR     PIC X(2).                                    
021300*                                 MFS ATTRIBUTFÄLT                        
021400     03 MOD-TEKRPLT          PIC X(20).                                   
021500*                                 GODS PLACERAT                           
021600*                                 GOODS PLACED                            
021700     03 MOD-TEMFSINF         PIC X(55).                                   
021800*                                 INFORMATIONSMEDDELANDE                  
021900*                                 INFORMATION MESSAGE                     
022000*** END OF VILMAII-COPY LENGTH= 659 BYTES                                 
