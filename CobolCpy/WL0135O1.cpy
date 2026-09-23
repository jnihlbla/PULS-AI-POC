000100 01  RESP-WL0135O1.                                                       
000200*                                 RESPONS FROM PGM WL0135                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 RESP-IDKVAINF-KEY    PIC 9(2).                                    
001000*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
001100*                                 LINENO FOR QUALITY CONTROL TEXT         
001200     03 RESP-TIREGDAT-KEY    PIC 9(6).                                    
001300*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001400*                                 REGISTRATION DATE (YYMMDD)              
001500     03 RESP-IDDC2-KEY       PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 RESP-TABELLRAD.                                                   
001900*                                 GRUPP MED TABELL RADER                  
002000        05 RESP-IDKVAINF-UPPD                                             
002100                             PIC 9(2).                                    
002200*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
002300*                                 LINENO FOR QUALITY CONTROL TEXT         
002400        05 RESP-IDNAMN       PIC X(26).                                   
002500*                                 NAMN                                    
002600        05 RESP-KDKVAINF     PIC X.                                       
002700*                                 TYP AV KVAL.INFO F÷R ARTIKEL            
002800*                                 TYPE OF QUAL.INFO. FOR PART             
002900        05 RESP-TIKLAR-LEV   PIC 9(6).                                    
003000*                                 KLARDATUM          (≈≈MMDD)             
003100*                                 READY DATE        (YYMMDD)              
003200        05 RESP-FLSTOCH      PIC X.                                       
003300*                                 STOCKCHECK FLAGGA                       
003400*                                                                         
003500*                                 STOCKCHECK FLAG                         
003600*                                                                         
003700        05 RESP-TIREGDAT     PIC 9(6).                                    
003800*                                 REGISTRERINGSDATUM (≈≈MMDD)             
003900*                                 REGISTRATION DATE (YYMMDD)              
004000        05 RESP-IDMAIL       PIC X(60).                                   
004100*                                 MAIL ADRESS                             
004200*                                 MAIL ADDRESS                            
004300        05 RESP-TEKVAINF-002-RAD1                                         
004400                             PIC X(79).                                   
004500*                                 KVALITETS INFORMATION                   
004600*                                 QUALITY INFORMATION PART NUMBER         
004700        05 RESP-TEKVAINF-002-RAD2                                         
004800                             PIC X(79).                                   
004900*                                 KVALITETS INFORMATION                   
005000*                                 QUALITY INFORMATION PART NUMBER         
005100        05 RESP-TEKVAINF-002-RAD3                                         
005200                             PIC X(79).                                   
005300*                                 KVALITETS INFORMATION                   
005400*                                 QUALITY INFORMATION PART NUMBER         
005500        05 RESP-TEKVAINF-002-RAD4                                         
005600                             PIC X(79).                                   
005700*                                 KVALITETS INFORMATION                   
005800*                                 QUALITY INFORMATION PART NUMBER         
005900        05 RESP-TEKVAINF-002-RAD5                                         
006000                             PIC X(79).                                   
006100*                                 KVALITETS INFORMATION                   
006200*                                 QUALITY INFORMATION PART NUMBER         
006300        05 RESP-TEKVAINF-002-RAD6                                         
006400                             PIC X(79).                                   
006500*                                 KVALITETS INFORMATION                   
006600*                                 QUALITY INFORMATION PART NUMBER         
006700        05 RESP-TEKVAINF-002-RAD7                                         
006800                             PIC X(79).                                   
006900*                                 KVALITETS INFORMATION                   
007000*                                 QUALITY INFORMATION PART NUMBER         
007100        05 RESP-TISTADAT-UT  PIC X(6).                                    
007200*                                 GENERELLT STARTDATUM                    
007300*                                 GENERAL START DATE                      
007400        05 RESP-TISTADAT-IN  PIC 9(6).                                    
007500*                                 GENERELLT STARTDATUM                    
007600*                                 GENERAL START DATE                      
007700        05 RESP-TISTODAT-UT  PIC X(6).                                    
007800*                                 GENERELLT STOPPDATUM                    
007900*                                 GENERAL STOP DATE YYMMDD                
008000        05 RESP-TISTODAT-IN  PIC 9(6).                                    
008100*                                 GENERELLT STOPPDATUM                    
008200*                                 GENERAL STOP DATE YYMMDD                
008300        05 RESP-KVANTAL-UT   PIC Z(5)9.                                   
008400*                                 ANTAL                                   
008500*                                 NUMBER                                  
008600        05 RESP-KVANTAL-IN   PIC Z(5)9.                                   
008700*                                 ANTAL                                   
008800*                                 NUMBER                                  
008900        05 RESP-KVAVV-KVAL-UT                                             
009000                             PIC Z(6)9.                                   
009100*                                 ANTALSAVVIKELSE KVALITET                
009200*                                 QUANTITYDEVIATION QUALITY               
009300        05 RESP-KVAVV-KVAL-IN                                             
009400                             PIC Z(6)9.                                   
009500*                                 ANTALSAVVIKELSE KVALITET                
009600*                                 QUANTITYDEVIATION QUALITY               
009700        05 RESP-KVART-SKROT-UT                                            
009800                             PIC Z(6)9.                                   
009900*                                 ANTAL SKROTADE ARTIKLAR                 
010000*                                 QUANTITY INSPECTED PARTS                
010100        05 RESP-KVART-SKROT-IN                                            
010200                             PIC Z(6)9.                                   
010300*                                 ANTAL SKROTADE ARTIKLAR                 
010400*                                 QUANTITY INSPECTED PARTS                
010500        05 RESP-KVART-RET-UT PIC Z(6)9.                                   
010600*                                 ANTAL ARTIKLAR I RETUR                  
010700*                                 QUANTITY INSPECTED PARTS                
010800        05 RESP-KVART-RET-IN PIC Z(6)9.                                   
010900*                                 ANTAL ARTIKLAR I RETUR                  
011000*                                 QUANTITY INSPECTED PARTS                
011100        05 RESP-KVART-KJUST-UT                                            
011200                             PIC Z(6)9.                                   
011300*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
011400*                                 QUANTITY INSPECTED PARTS                
011500        05 RESP-KVART-KJUST-IN                                            
011600                             PIC Z(6)9.                                   
011700*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
011800*                                 QUANTITY INSPECTED PARTS                
011900        05 RESP-BEINIT       PIC X(3).                                    
012000*                                 INITIALER F÷R EN PERSON                 
012100*                                 INITIALS FOR A PERSON                   
012200        05 RESP-IDNAMN-DC-QUAL                                            
012300                             PIC X(21).                                   
012400*                                 NAMN                                    
012500        05 RESP-TEKVAINF-DC-RAD1                                          
012600                             PIC X(79).                                   
012700*                                 KVALITETS INFORMATION                   
012800*                                 QUALITY INFORMATION PART NUMBER         
012900        05 RESP-TEKVAINF-DC-RAD2                                          
013000                             PIC X(79).                                   
013100*                                 KVALITETS INFORMATION                   
013200*                                 QUALITY INFORMATION PART NUMBER         
013300        05 RESP-TEKVAINF-DC-RAD3                                          
013400                             PIC X(79).                                   
013500*                                 KVALITETS INFORMATION                   
013600*                                 QUALITY INFORMATION PART NUMBER         
013700        05 RESP-TEKVAINF-DC-RAD4                                          
013800                             PIC X(79).                                   
013900*                                 KVALITETS INFORMATION                   
014000*                                 QUALITY INFORMATION PART NUMBER         
014100        05 RESP-FLTABORT     PIC X.                                       
014200*                                 BORTTAGSFLAGGA                          
014300*                                 DELETE FLAG                             
014400*** END OF VILMAII-COPY LENGTH= 1108 BYTES                                
