000100 01  KR-RDH701.                                                           
000200*                                 KVALITET                                
000300*                                 KONTROLLRAPPORT                         
000400*                                 FYSISK NYCKEL: IDKR                     
000500     03 KR-IDPTYP            PIC X(3).                                    
000600*                                 POSTTYP                                 
000700*                                 RECORD TYPE                             
000800     03 KR-IDKR              PIC X(7).                                    
000900*                                 KONTROLLRAPPORT NUMMER                  
001000*                                 INSPECTION REPORT NUMBER                
001100     03 KR-ADATTENT          PIC X(40).                                   
001200*                                 ATTENTIONADRESS                         
001300*                                 ATTENTION ADDRESS                       
001400     03 KR-BEKRANS           PIC X(25).                                   
001500*                                 ANSVARIG                                
001600*                                                                         
001700*                                 RESPONSIBLE                             
001800*                                                                         
001900     03 KR-BEKRBEH           PIC X(25).                                   
002000*                                 KONTROLLANT                             
002100*                                                                         
002200     03 KR-BEKRPACK          PIC X(25).                                   
002300*                                 ANSVARIG FÖR PACKNING                   
002400*                                                                         
002500*                                 RESPONSIBLE FOR PACKING                 
002600*                                                                         
002700     03 KR-FLKRATG           PIC X.                                       
002800*                                 ÅTGÄRD BEGÄRD FÖR LEV.BER.FEL           
002900*                                 LONG-ENG ALIAS                          
003000     03 KR-FLKRGODK          PIC X.                                       
003100*                                 GOKDKÄND                                
003200*                                 APPROVED                                
003300     03 KR-FLKRLFEL          PIC X.                                       
003400*                                 LEVERANTÖRSBEROENDE FEL                 
003500*                                 SUPPLIER ERROR                          
003600     03 KR-FLKROMK           PIC X.                                       
003700*                                 OMKOSTNADER KLAR FÖR DEB AV LEV         
003800*                                 LONG-ENG ALIAS                          
003900     03 KR-IDANALYSNR        PIC S9(9)           COMP-3.                  
004000*                                 ANALYSNUMMER                            
004100     03 KR-IDARTNR           PIC S9(9)           COMP-3.                  
004200*                                 ARTIKELNUMMER                           
004300*                                 PART NUMBER                             
004400     03 KR-IDAVINR           PIC S9(7)           COMP-3.                  
004500*                                 AVI-NUMMER                              
004600*                                 ADVICE NOTE NUMBER                      
004700     03 KR-IDFTG             PIC 9(2).                                    
004800*                                 FÖRETAGSID EKONOM REDOVISNING           
004900*                                 COMPANY IDENTITY ACCOUNTING             
005000     03 KR-IDKONTO           PIC S9(11)          COMP-3.                  
005100*                                 KONTO                                   
005200*                                 ACCOUNT                                 
005300     03 KR-IDKRATLF          PIC X(20).                                   
005400*                                 TELEFON TILL ANSVARIG                   
005500*                                                                         
005600*                                 TELEPHONE TO RESPONSIBLE                
005700*                                                                         
005800     03 KR-IDKRFEL           OCCURS 3 TIMES                               
005900                             PIC X(2).                                    
006000*                                 FELKOD FÖR KONTROLLRAPPORT              
006100*                                 ERRORCODE FOR INSP.REPORT               
006200     03 KR-IDLEVG            PIC S9(5)           COMP-3.                  
006300*                                 LEVERANTÖRS GODSADRESS NUMMER           
006400*                                 SUPPLIER WAREHOUSE NUMBER               
006500     03 KR-IDLEVNR           PIC S9(5)           COMP-3.                  
006600*                                 LEVERANTÖRNUMMER                        
006700*                                 SUPPLIER NUMBER                         
006800     03 KR-IDLOPNRM          PIC S9(9)           COMP-3.                  
006900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
007000*                                 (0VVDLLLLK)                             
007100*                                 CURRENCY NO                             
007200*                                 (0WWDLLLLC)                             
007300     03 KR-IDSKYLT           PIC X(3).                                    
007400*                                 NATIONALITETSTECKEN                     
007500*                                 NATIONALITY SIGN                        
007600     03 KR-KDCLAGER          PIC S9              COMP-3.                  
007700*                                 CENTRALLAGERKOD                         
007800*                                 CENTRAL WAREHOUSE CODE                  
007900     03 KR-KDKRSTA           PIC X.                                       
008000*                                 KONTROLLRAPPORT STATUS                  
008100*                                 INSPECTION REPORT STATUS                
008200     03 KR-KVANTMOT          PIC S9(7)           COMP-3.                  
008300*                                 ANTAL MOTTAGET                          
008400*                                 QUANTITY RECEIVED                       
008500     03 KR-KVARBTID          PIC S9(2)V9(1)      COMP-3.                  
008600*                                 ANTAL MANTIMMAR                         
008700*                                 NUMBER OF MAN HOURS                     
008800     03 KR-KVART-AAVV        PIC S9(7)           COMP-3.                  
008900*                                 ANTALSAVVIKELSE FÖR ARTIKEL             
009000*                                 QUANTITY INSPECTED PARTS                
009100     03 KR-KVART-BEH         PIC S9(7)           COMP-3.                  
009200*                                 ANTAL ARTIKLAR SOM BEHÅLLES             
009300*                                 QUANTITY INSPECTED PARTS                
009400     03 KR-KVART-EJ-GODK     PIC S9(7)           COMP-3.                  
009500*                                 ANTAL EJ GODKÄNDA ARTIKLAR              
009600*                                 QUANTITY INSPECTED PARTS                
009700     03 KR-KVART-KJUST       PIC S9(7)           COMP-3.                  
009800*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
009900*                                 QUANTITY INSPECTED PARTS                
010000     03 KR-KVART-KONTR       PIC S9(7)           COMP-3.                  
010100*                                 ANTAL KONTROLLERAD ARTIKLAR             
010200*                                 QUANTITY INSPECTED PARTS                
010300     03 KR-KVART-RET         PIC S9(7)           COMP-3.                  
010400*                                 ANTAL ARTIKLAR I RETUR                  
010500*                                 QUANTITY INSPECTED PARTS                
010600     03 KR-KVART-SJUST       PIC S9(7)           COMP-3.                  
010700*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
010800*                                 QUANTITY INSPECTED PARTS                
010900     03 KR-KVART-SKROT       PIC S9(7)           COMP-3.                  
011000*                                 ANTAL SKROTADE ARTIKLAR                 
011100*                                 QUANTITY INSPECTED PARTS                
011200     03 KR-KVAVIS            PIC S9(7)           COMP-3.                  
011300*                                 AVISERAT ANTAL                          
011400*                                 QUANTITY NOTIFIED                       
011500     03 KR-KVKRBEH           PIC 9(2)V9(1).                               
011600*                                 BEHANDLINGSTID                          
011700*                                 LONG-ENG ALIAS                          
011800     03 KR-KVKRKNTR          PIC S9              COMP-3.                  
011900*                                 REKNEVERK ANTAL/KVALITET AVV            
012000*                                 COUNTER QUANTITY/QUALITY DEV            
012100     03 KR-KVKRPACK          PIC 9(2)V9(1).                               
012200*                                 PACKNINGSTID                            
012300*                                 TIME FOR PACKING                        
012400     03 KR-SUMAT             PIC S9(7)V9(2)      COMP-3.                  
012500*                                 MATERIALKOSTNAD                         
012600     03 KR-SUOMK             PIC S9(7)           COMP-3.                  
012700*                                 BELOPP SOM SKALL DEBITERAS              
012800*                                 KUND                                    
012900*                                 AMOUNT TO BE PAID BY CUSTOMER           
013000     03 KR-TEKRFEL           PIC X(70).                                   
013100*                                 FELBESKRIVNING I FRI TEXT               
013200*                                                                         
013300*                                 DESCRIP.ERROR IN .... TEXT              
013400*                                                                         
013500     03 KR-TEKRPLT           PIC X(20).                                   
013600*                                 GODS PLACERAT                           
013700*                                 LONG-ENG ALIAS                          
013800     03 KR-TEKRSPEC-ATID     PIC X(40).                                   
013900*                                 SPECIFIKATION ARBETSTID                 
014000*                                 LONG-ENG ALIAS                          
014100     03 KR-TEKRSPEC-MAT      PIC X(40).                                   
014200*                                 SPECIFIKATION MATERIALKOSTNAD           
014300*                                 LONG-ENG ALIAS                          
014400     03 KR-TEKRSPEC-OMK      PIC X(40).                                   
014500*                                 SPECIFIKATION ARBETSKOSTNAD             
014600*                                 LONG-ENG ALIAS                          
014700     03 KR-TIAVSDAT          PIC S9(7)           COMP-3.                  
014800*                                 AVISERINGSDATUM  (ÅÅMMDD)               
014900     03 KR-TIKRPACK          PIC S9(7)           COMP-3.                  
015000*                                 PACKNINGSDATUM                          
015100*                                 DATE OF PACKING                         
015200     03 KR-TIREGDAT          PIC S9(7)           COMP-3.                  
015300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
015400*                                 REGISTRATION DATE (YYMMDD)              
015500*** END COPY W426020     LENGTH=473                                       
