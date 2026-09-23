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
002700     03 KR-KDKRATG           PIC X.                                       
002800*                                 ÅTGÄRD BEGÄRD FÖR LEV.BER.FEL           
002900*                                 MEASURES CRAVED FOR SUPPL.ERROR         
003000     03 KR-FLINKANS          PIC X.                                       
003100*                                 ÅTGÄRDSANSVAR INKÖP                     
003200*                                 ACTION-RESPONSIBILITY PURCHASER         
003300     03 KR-FLKRGODK          PIC X.                                       
003400*                                 GOKDKÄND                                
003500*                                 APPROVED                                
003600     03 KR-FLKRLFEL          PIC X.                                       
003700*                                 LEVERANTÖRSBEROENDE FEL                 
003800*                                 SUPPLIER ERROR                          
003900     03 KR-FLKROMK           PIC X.                                       
004000*                                 OMKOSTNADER KLAR FÖR DEB AV LEV         
004100*                                 COSTS READY TO DEBIT SUPPLIER           
004200     03 KR-IDANALYSNR        PIC S9(9)           COMP-3.                  
004300*                                 ANALYSNUMMER                            
004400     03 KR-IDARTNR           PIC S9(9)           COMP-3.                  
004500*                                 ARTIKELNUMMER                           
004600*                                 PART NUMBER                             
004700     03 KR-IDAVINR           PIC S9(7)           COMP-3.                  
004800*                                 AVI-NUMMER                              
004900*                                 ADVICE NOTE NUMBER                      
005000     03 KR-IDFTG             PIC 9(2).                                    
005100*                                 FÖRETAGSID EKONOM REDOVISNING           
005200*                                 COMPANY IDENTITY ACCOUNTING             
005300     03 KR-IDKONTO           PIC S9(11)          COMP-3.                  
005400*                                 KONTO                                   
005500*                                 ACCOUNT                                 
005600     03 KR-IDKRATLF          PIC X(20).                                   
005700*                                 TELEFON TILL ANSVARIG                   
005800*                                                                         
005900*                                 TELEPHONE TO RESPONSIBLE                
006000*                                                                         
006100     03 KR-IDKRFEL           PIC X(2).                                    
006200*                                 FELKOD FÖR KONTROLLRAPPORT              
006300*                                 ERRORCODE FOR INSP.REPORT               
006400     03 KR-IDLEVG            PIC S9(5)           COMP-3.                  
006500*                                 LEVERANTÖRS GODSADRESS NUMMER           
006600*                                 SUPPLIER WAREHOUSE NUMBER               
006700     03 KR-IDLEVNR           PIC S9(5)           COMP-3.                  
006800*                                 LEVERANTÖRNUMMER                        
006900*                                 SUPPLIER NUMBER (VENDORNUMBER)          
007000     03 KR-IDLOPNRM          PIC S9(9)           COMP-3.                  
007100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
007200*                                 (0VVDLLLLK)                             
007300*                                 SERIAL NO RECEIVING REPORT              
007400*                                 (0WWDLLLLC)                             
007500     03 KR-IDDC              PIC X(2).                                    
007600*                                 IDENTIFIERARE LAGER                     
007700*                                 WAREHOUSE IDENTIFIER                    
007800     03 KR-KDDISP            PIC 9(2).                                    
007900*                                 DISPOSITION CODE                        
008000*                                 DISPOSITION CODE                        
008100     03 KR-KDHANDCO          PIC 9.                                       
008200*                                 OMKOSTNADSKOD                           
008300*                                 HANDLING COST CODE                      
008400     03 KR-KDKRJUST          PIC X.                                       
008500*                                 JUSTERINGSKOD                           
008600*                                 ADJUSTMENT CODE                         
008700     03 KR-KDKRSTA           PIC X.                                       
008800*                                 KONTROLLRAPPORT STATUS                  
008900*                                 INSPECTION REPORT STATUS                
009000     03 KR-KDKRUTF           PIC X.                                       
009100*                                 UTFÖRANDEKOD FÖR KONTROLLRAPP.          
009200*                                 INSPECTION REPORT CODE                  
009300     03 KR-KVANTMOT          PIC S9(7)           COMP-3.                  
009400*                                 ANTAL MOTTAGET                          
009500*                                 QUANTITY RECEIVED                       
009600     03 KR-KVARBTID          PIC S9(2)V9(1)      COMP-3.                  
009700*                                 ANTAL MANTIMMAR                         
009800*                                 NUMBER OF MAN HOURS                     
009900     03 KR-KVART-AAVV        PIC S9(7)           COMP-3.                  
010000*                                 ANTALSAVVIKELSE FÖR ARTIKEL             
010100*                                 QUANTITY INSPECTED PARTS                
010200     03 KR-KVART-BEH         PIC S9(7)           COMP-3.                  
010300*                                 ANTAL ARTIKLAR SOM BEHÅLLES             
010400*                                 QUANTITY INSPECTED PARTS                
010500     03 KR-KVART-EJ-GODK     PIC S9(7)           COMP-3.                  
010600*                                 ANTAL EJ GODKÄNDA ARTIKLAR              
010700*                                 QUANTITY INSPECTED PARTS                
010800     03 KR-KVART-KJUST       PIC S9(7)           COMP-3.                  
010900*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
011000*                                 QUANTITY INSPECTED PARTS                
011100     03 KR-KVART-KONTR       PIC S9(7)           COMP-3.                  
011200*                                 ANTAL KONTROLLERAD ARTIKLAR             
011300*                                 QUANTITY INSPECTED PARTS                
011400     03 KR-KVART-RET         PIC S9(7)           COMP-3.                  
011500*                                 ANTAL ARTIKLAR I RETUR                  
011600*                                 QUANTITY INSPECTED PARTS                
011700     03 KR-KVART-SJUST       PIC S9(7)           COMP-3.                  
011800*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
011900*                                 QUANTITY INSPECTED PARTS                
012000     03 KR-KVART-SKROT       PIC S9(7)           COMP-3.                  
012100*                                 ANTAL SKROTADE ARTIKLAR                 
012200*                                 QUANTITY INSPECTED PARTS                
012300     03 KR-KVAVIS            PIC S9(7)           COMP-3.                  
012400*                                 AVISERAT ANTAL                          
012500*                                 QUANTITY NOTIFIED                       
012600     03 KR-KVKRBEH           PIC 9(2)V9(1).                               
012700*                                 BEHANDLINGSTID FÖR KR                   
012800*                                 USED TIME FOR INSP. REPORT              
012900     03 KR-KVKRKNTR          PIC S9              COMP-3.                  
013000*                                 REKNEVERK ANTAL/KVALITET AVV            
013100*                                 COUNTER QUANTITY/QUALITY DEV            
013200     03 KR-KVKRPACK          PIC 9(2)V9(1).                               
013300*                                 PACKNINGSTID                            
013400*                                 TIME FOR PACKING                        
013500     03 KR-SUMAT             PIC S9(7)V9(2)      COMP-3.                  
013600*                                 MATERIALKOSTNAD                         
013700     03 KR-SUOMK             PIC S9(7)           COMP-3.                  
013800*                                 BELOPP SOM SKALL DEBITERAS              
013900*                                 KUND                                    
014000*                                 AMOUNT TO BE PAID BY CUSTOMER           
014100     03 KR-TEKRPLT           PIC X(20).                                   
014200*                                 GODS PLACERAT                           
014300*                                 GOODS PLACED                            
014400     03 KR-TEKRSPEC-ATID     PIC X(30).                                   
014500*                                 SPECIFIKATION ARBETSTID                 
014600*                                 SPECIFICATION WORKING-HOURS             
014700     03 KR-TEKRSPEC-MAT      PIC X(30).                                   
014800*                                 SPECIFIKATION MATERIALKOSTNAD           
014900*                                 SPECIFICATION MATERIAL COSTS            
015000     03 KR-TEKRSPEC-OMK      PIC X(30).                                   
015100*                                 SPECIFIKATION OMKOSTNADER               
015200*                                 SPECIFICATION INDIRECT COSTS            
015300     03 KR-TIAVSDAT          PIC S9(7)           COMP-3.                  
015400*                                 AVISERINGSDATUM (YYMMDD)                
015500*                                 ADVICE NOTE DATE                        
015600     03 KR-TIKRANS           PIC S9(7)           COMP-3.                  
015700*                                 DATUM KONTROLLRAPPORT GODKÄND           
015800*                                 DATE INSP.REPORT APPROVED               
015900     03 KR-TIKRPACK          PIC S9(7)           COMP-3.                  
016000*                                 PACKNINGSDATUM                          
016100*                                 DATE OF PACKING                         
016200     03 KR-TIREGDAT          PIC S9(7)           COMP-3.                  
016300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
016400*                                 REGISTRATION DATE (YYMMDD)              
016500*** END OF VILMAII-COPY LENGTH= 373 BYTES                                 
