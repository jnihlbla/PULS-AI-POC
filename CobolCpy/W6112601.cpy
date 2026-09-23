000100 01  W61126.                                                              
000200*                                 INLEVERANS PARTIINFO                    
000300*                                 NYCKEL=IDLOPNRM                         
000400*                                                          .              
000500*                                 PARTS RECEIVING LOT INFO.               
000600*                                 KEY=IDLOPNRM                            
000700*                                                          .              
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
001200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001300*                                 (0VVDLLLLK)                             
001400*                                 SERIAL NO RECEIVING REPORT              
001500*                                 (0WWDLLLLC)                             
001600     03 TIAAVVD              PIC S9(5)           COMP-3.                  
001700*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
001800*                                 YEAR - WEEK - DAY  (YYWWD)              
001900     03 TIPP                 PIC S9(2)           COMP-3.                  
002000*                                 PLANERINGSPERIOD (PP)                   
002100*                                 12 PER ÅR                               
002200*                                 PLANNING PERIOD  (PP)                   
002300*                                 12 PER YEAR                             
002400     03 W611R31.                                                          
002500        05 IDPTYP            PIC X(3).                                    
002600*                                 POSTTYP                                 
002700*                                 RECORD TYPE                             
002800        05 IDARTNR           PIC S9(9)           COMP-3.                  
002900*                                 ARTIKELNUMMER                           
003000*                                 PART NUMBER                             
003100        05 IDLEVNR-INL       PIC X(5).                                    
003200*                                 LEVERANTÖR FÖR AKTUELL INLEV.           
003300        05 KDRT              PIC S9(3)           COMP-3.                  
003400*                                 REDOVISNINGSTYP                         
003500*                                 TYPE OF ACCOUNTING                      
003600        05 TIAVIDAT          PIC S9(7)           COMP-3.                  
003700*                                 AVISERINGSDATUM (YYMMDD)                
003800*                                 ADVICE NOTE DATE                        
003900        05 IDKONTO           PIC S9(11)          COMP-3.                  
004000*                                 KONTO                                   
004100*                                 ACCOUNT                                 
004200        05 IDKST             PIC X(10).                                   
004300*                                 KOSTNADSSTÄLLE                          
004400*                                 COST CENTRE                             
004500        05 IDANALYS          PIC X(12).                                   
004600*                                 ANALYSNUMMER                            
004700*                                 ANALYSIS NUMBER                         
004800        05 IDFTG             PIC 9(2).                                    
004900*                                 FÖRETAGSID EKONOM REDOVISNING           
005000*                                 COMPANY IDENTITY ACCOUNTING             
005100        05 IDFS              PIC X(8).                                    
005200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
005300*                                 ADVICE NOTE NUMBER ODETTE               
005400        05 KVAVIS            PIC S9(7)           COMP-3.                  
005500*                                 AVISERAT ANTAL                          
005600*                                 QUANTITY NOTIFIED                       
005700        05 KDAVVANT          PIC S9              COMP-3.                  
005800*                                 AVVIKELSEANTAL KOD                      
005900*                                 0=INGEN ANM.   1=AVVIKELSE              
006000*                                 2=MAKULERING AV MOTT.RAPPORT            
006100*                                 QUANTITY DEVIATION  CODE                
006200*                                 0=NO DEV.    1=DEVIATION                
006300*                                 2=CANCELLING OF REC. REPORT             
006400        05 KVANTMOT          PIC S9(7)           COMP-3.                  
006500*                                 ANTAL MOTTAGET                          
006600*                                 QUANTITY RECEIVED                       
006700        05 KDAVVKV           PIC S9              COMP-3.                  
006800*                                 KVALITETSAVVIKELSEKOD                   
006900*                                 0=INGEN ANM.  1=AVVIKELSE               
007000*                                 2=AVVIKELSE, RETURNERAS                 
007100*                                 QUALITY DEVIATION CODE                  
007200*                                 0 = NO DEV.  1 = DEVIATION              
007300*                                 2 = DEVIATION, WILL BE RETURNED         
007400        05 KVRETUR           PIC S9(7)           COMP-3.                  
007500*                                 ANTAL I RETUR                           
007600*                                 QUANTITY IN RETURN                      
007700        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
007800*                                 LAGEROMRÅDE                             
007900*                                 AREA                                    
008000        05 BEFT              PIC S9(3)           COMP-3.                  
008100*                                 FÖRPACKNINGSTYP                         
008200*                                 PACKAGING TYPE                          
008300        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
008400*                                 ARTIKELSTANDARDPRIS                     
008500*                                 STANDARD PRICE                          
008600        05 KDFORP            PIC S9(5)           COMP-3.                  
008700*                                 FÖRPACKNINGSKOD                         
008800*                                 PACKAGING CODE                          
008900        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
009000*                                 SEPARAT PERIODBEHOV                     
009100*                                 SEPARATE PERIOD REQUIREMENTS            
009200        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
009300*                                 SATS-PERIODBEHOV                        
009400*                                 KIT PERIOD REQUIREMENTS                 
009500        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
009600*                                 EMBALLAGEKOD 0                          
009700        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
009800*                                 EMBALLAGEKOD 1                          
009900        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
010000*                                 EMBALLAGEKOD 2                          
010100        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
010200*                                 ANTAL I Q0 FÖRPACKNING                  
010300        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
010400*                                 ANTAL I Q1 FÖRPACKNING                  
010500*                                 QUANTITY IN BULK PACK Q1                
010600        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
010700*                                 ANTAL I Q2 FÖRPACKNING                  
010800*                                 QUANTITY IN BULK PACK Q2                
010900        05 KVKOLLI           PIC S9(5)           COMP-3.                  
011000*                                 ANTAL KOLLI                             
011100*                                 NBR OF CASES                            
011200        05 KVKOLLI-RAPP      PIC S9(5)           COMP-3.                  
011300*                                 ANTAL KOLLI                             
011400*                                 NBR OF CASES                            
011500        05 KVRAPP            PIC S9(7)           COMP-3.                  
011600*                                 DELRAPPORTERAT ANTAL                    
011700*                                 PARTIAL REPORTED QUANTITY               
011800*** END OF VILMAII-COPY LENGTH= 128 BYTES                                 
