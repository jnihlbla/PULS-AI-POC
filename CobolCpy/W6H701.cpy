000100 01  KR-W6H701.                                                           
000200*                                 KVALITET                                
000300*                                 KONTROLLRAPPORT                         
000400*                                 FYSISK NYCKEL: IDKR                     
000500     03 KR-IDKR              PIC 9(5).                                    
000600*                                 KONTROLLRAPPORT NUMMER                  
000700*                                 INSPECTION REPORT NUMBER                
000800     03 KR-ADATTENT          PIC X(40).                                   
000900*                                 ATTENTIONADRESS                         
001000*                                 ATTENTION ADDRESS                       
001100     03 KR-BEKRANS           PIC X(25).                                   
001200*                                 ANSVARIG                                
001300*                                                                         
001400*                                 RESPONSIBLE                             
001500*                                                                         
001600     03 KR-BEKRBEH           PIC X(25).                                   
001700*                                 KONTROLLANT                             
001800*                                 INSPECTOR                               
001900     03 KR-BEKRPACK          PIC X(25).                                   
002000*                                 ANSVARIG FÖR PACKNING                   
002100*                                                                         
002200*                                 RESPONSIBLE FOR PACKING                 
002300*                                                                         
002400     03 KR-FLANNULL          PIC X.                                       
002500*                                 ANNULLATION                             
002600*                                 CANCELLATION                            
002700     03 KR-FLARBDEB          PIC X.                                       
002800*                                 ARB-KOST DEBITERAS J/N                  
002900     03 KR-FLINKANS          PIC X.                                       
003000*                                 ÅTGÄRDSANSVAR INKÖP                     
003100*                                 ACTION-RESPONSIBILITY PURCHASER         
003200     03 KR-FLKRGODK          PIC X.                                       
003300*                                 GOKDKÄND                                
003400*                                 APPROVED                                
003500     03 KR-FLKRLFEL          PIC X.                                       
003600*                                 LEVERANTÖRSBEROENDE FEL                 
003700*                                 SUPPLIER ERROR                          
003800     03 KR-FLKROMK           PIC X.                                       
003900*                                 OMKOSTNADER KLAR FÖR DEB AV LEV         
004000*                                 COSTS READY TO DEBIT SUPPLIER           
004100     03 KR-IDARTNR           PIC S9(9)           COMP-3.                  
004200*                                 ARTIKELNUMMER                           
004300*                                 PART NUMBER                             
004400     03 KR-IDAVINR           PIC S9(7)           COMP-3.                  
004500*                                 AVI-NUMMER                              
004600*                                 ADVICE NOTE NUMBER                      
004700     03 KR-IDFTG             PIC 9(2).                                    
004800*                                 FÖRETAGSID EKONOM REDOVISNING           
004900*                                 COMPANY IDENTITY ACCOUNTING             
005000     03 KR-IDKRATLF          PIC X(20).                                   
005100*                                 TELEFON TILL ANSVARIG                   
005200*                                                                         
005300*                                 TELEPHONE TO RESPONSIBLE                
005400*                                                                         
005500     03 KR-IDKRFEL           PIC X(2).                                    
005600*                                 FELKOD FÖR KONTROLLRAPPORT              
005700*                                 ERRORCODE FOR INSP.REPORT               
005800     03 KR-IDLEVG            PIC S9(5)           COMP-3.                  
005900*                                 LEVERANTÖRS GODSADRESS NUMMER           
006000*                                 SUPPLIER WAREHOUSE NUMBER               
006100     03 KR-IDLEVNR           PIC X(5).                                    
006200*                                 LEVERANTÖRNUMMER                        
006300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
006400     03 KR-IDLOPNRM          PIC S9(9)           COMP-3.                  
006500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
006600*                                 (0VVDLLLLK)                             
006700*                                 SERIAL NO RECEIVING REPORT              
006800*                                 (0WWDLLLLC)                             
006900     03 KR-IDDC              PIC X(2).                                    
007000*                                 IDENTIFIERARE LAGER                     
007100*                                 WAREHOUSE IDENTIFIER                    
007200     03 KR-KDDISP            PIC 9(2).                                    
007300*                                 DISPOSITION CODE                        
007400*                                 DISPOSITION CODE                        
007500     03 KR-KDHANDCO          PIC 9.                                       
007600*                                 OMKOSTNADSKOD                           
007700*                                 HANDLING COST CODE                      
007800     03 KR-KDKRATG           PIC X.                                       
007900*                                 ÅTGÄRD BEGÄRD FÖR LEV.BER.FEL           
008000*                                 MEASURES CRAVED FOR SUPPL.ERROR         
008100     03 KR-KDKRJUST          PIC X.                                       
008200*                                 JUSTERINGSKOD                           
008300*                                 ADJUSTMENT CODE                         
008400     03 KR-KDKRSTA           PIC X.                                       
008500*                                 KONTROLLRAPPORT STATUS                  
008600*                                 INSPECTION REPORT STATUS                
008700     03 KR-KDKRUTF           PIC X.                                       
008800*                                 UTFÖRANDEKOD FÖR KONTROLLRAPP.          
008900*                                 INSPECTION REPORT CODE                  
009000     03 KR-KVANTMOT          PIC S9(7)           COMP-3.                  
009100*                                 ANTAL MOTTAGET                          
009200*                                 QUANTITY RECEIVED                       
009300     03 KR-KVARBTID          PIC S9(2)V9(1)      COMP-3.                  
009400*                                 ANTAL MANTIMMAR                         
009500*                                 NUMBER OF MAN HOURS                     
009600     03 KR-KVART-AAVV        PIC S9(7)           COMP-3.                  
009700*                                 ANTALSAVVIKELSE FÖR ARTIKEL             
009800*                                 QUANTITY INSPECTED PARTS                
009900     03 KR-KVART-BEH         PIC S9(7)           COMP-3.                  
010000*                                 ANTAL ARTIKLAR SOM BEHÅLLES             
010100*                                 QUANTITY INSPECTED PARTS                
010200     03 KR-KVART-EJ-GODK     PIC S9(7)           COMP-3.                  
010300*                                 ANTAL EJ GODKÄNDA ARTIKLAR              
010400*                                 QUANTITY INSPECTED PARTS                
010500     03 KR-KVART-KJUST       PIC S9(7)           COMP-3.                  
010600*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
010700*                                 QUANTITY INSPECTED PARTS                
010800     03 KR-KVART-KONTR       PIC S9(7)           COMP-3.                  
010900*                                 ANTAL KONTROLLERAD ARTIKLAR             
011000*                                 QUANTITY INSPECTED PARTS                
011100     03 KR-KVART-RET         PIC S9(7)           COMP-3.                  
011200*                                 ANTAL ARTIKLAR I RETUR                  
011300*                                 QUANTITY INSPECTED PARTS                
011400     03 KR-KVART-SJUST       PIC S9(7)           COMP-3.                  
011500*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
011600*                                 QUANTITY INSPECTED PARTS                
011700     03 KR-KVART-SKROT       PIC S9(7)           COMP-3.                  
011800*                                 ANTAL SKROTADE ARTIKLAR                 
011900*                                 QUANTITY INSPECTED PARTS                
012000     03 KR-KVAVIS            PIC S9(7)           COMP-3.                  
012100*                                 AVISERAT ANTAL                          
012200*                                 QUANTITY NOTIFIED                       
012300     03 KR-KVKRBEH           PIC 9(2)V9(1).                               
012400*                                 BEHANDLINGSTID FÖR KR                   
012500*                                 USED TIME FOR INSP. REPORT              
012600     03 KR-KVKRKNTR          PIC S9              COMP-3.                  
012700*                                 REKNEVERK ANTAL/KVALITET AVV            
012800*                                 COUNTER QUANTITY/QUALITY DEV            
012900     03 KR-KVKRPACK          PIC 9(2)V9(1).                               
013000*                                 PACKNINGSTID                            
013100*                                 TIME FOR PACKING                        
013200     03 KR-SUMAT             PIC S9(7)V9(2)      COMP-3.                  
013300*                                 MATERIALKOSTNAD                         
013400     03 KR-SUOMK             PIC S9(7)           COMP-3.                  
013500*                                 SUMMA OMKOSTNADER                       
013600*                                 AMOUNT OF COST                          
013700     03 KR-TEKRPLT           PIC X(20).                                   
013800*                                 GODS PLACERAT                           
013900*                                 GOODS PLACED                            
014000     03 KR-TEKRSPEC-ATID     PIC X(30).                                   
014100*                                 SPECIFIKATION ARBETSTID                 
014200*                                 SPECIFICATION WORKING-HOURS             
014300     03 KR-TEKRSPEC-MAT      PIC X(30).                                   
014400*                                 SPECIFIKATION MATERIALKOSTNAD           
014500*                                 SPECIFICATION MATERIAL COSTS            
014600     03 KR-TEKRSPEC-OMK      PIC X(30).                                   
014700*                                 SPECIFIKATION OMKOSTNADER               
014800*                                 SPECIFICATION INDIRECT COSTS            
014900     03 KR-DAAVSDAT          PIC 9(8).                                    
015000*                                 AVISERINGSDATUM (YYYYMMDD)              
015100*                                 ADVICE NOTE DATE                        
015200     03 KR-TIKRANS           PIC S9(7)           COMP-3.                  
015300*                                 DATUM KONTROLLRAPPORT GODKÄND           
015400*                                 DATE INSP.REPORT APPROVED               
015500     03 KR-TIKRPACK          PIC S9(7)           COMP-3.                  
015600*                                 PACKNINGSDATUM                          
015700*                                 DATE OF PACKING                         
015800     03 KR-DAREGDAT-9KOMPL   PIC 9(8).                                    
015900*                                 DATUMETS 9-KOMPLEMENT                   
016000*                                 DATES 9-COMPLEMENT                      
016100     03 KR-KVART-TIDGK       PIC S9(7)           COMP-3.                  
016200*                                 ANTAL TIDIGARE GODKÄNDA ARTIKLA         
016300*                                 R                                       
016400*                                 QUANTITY INSPECTED PARTS                
016500     03 KR-KDFAXVAL          PIC X.                                       
016600*                                 VAL AV FAXNUMMER                        
016700*                                 DEDICATED FAX NUMBER                    
016800     03 KR-KDMEMVAL          PIC X.                                       
016900*                                 VAL AV MEMOADRESS                       
017000*                                 DEDICATED MEMO ADDRESS                  
017100     03 KR-FLBUFJUS          PIC X.                                       
017200*                                 BUFFERTJUSERING                         
017300*                                 ADJUST BUFFER                           
017400     03 KR-FLKVALSP          PIC X.                                       
017500*                                 KVALITETSBLOCK JUSTERAS                 
017600*                                 QUANLITY BLOCK ADJUSTMENT               
017700     03 KR-KVART-SKROT-LDC   PIC S9(7)           COMP-3.                  
017800*                                 ANTAL SKROTADE ARTIKLAR LDC             
017900*                                 QUANTITY INSPECTED PARTS LDC            
018000     03 KR-FLEJKNTRL         PIC X.                                       
018100*                                 KVALITET KONTROLL FLAGGA                
018200*                                 QUALITY CONTROL FLAG                    
018300     03 KR-FLKRLIM           PIC X.                                       
018400*                                 FLAGGA LÅGT VÄRDE                       
018500*                                 FLAG LOW VALUE                          
018600     03 KR-SUKRLIM           PIC S9(3)V9(2)      COMP-3.                  
018700*                                 SUMMAGRÄNS LÅGT VÄRDE                   
018800*                                 SUM LIMIT LOW VALUE                     
018900     03 KR-FILLER            PIC X(5).                                    
019000*** END OF VILMAII-COPY LENGTH= 395 BYTES                                 
