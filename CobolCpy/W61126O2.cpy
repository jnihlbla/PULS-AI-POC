000100 01  W61126.                                                              
000110* DENNA COPYTEXT GÄLLER FÖR FILERNA: W020.VAAVV.ARHIST                    
000120* SOM SKAPADES FÖRE SAP-INSTALLATIONEN(NOVEMBER -98)                      
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
001900     03 TIPER-PLAN           PIC S9              COMP-3.                  
002000*                                 PLANERINGSPERIOD                        
002100     03 TIPER-RED            PIC S9              COMP-3.                  
002200*                                 REDOVISNINGSPERIOD                      
002300     03 W611R31.                                                          
002400        05 IDPTYP            PIC X(3).                                    
002500*                                 POSTTYP                                 
002600*                                 RECORD TYPE                             
002700        05 IDARTNR           PIC S9(9)           COMP-3.                  
002800*                                 ARTIKELNUMMER                           
002900*                                 PART NUMBER                             
003000        05 IDLEVNR-INL       PIC S9(5)           COMP-3.                  
003100*                                 LEVERANTÖR FÖR AKTUELL INLEV.           
003200        05 KDRT              PIC S9(3)           COMP-3.                  
003300*                                 REDOVISNINGSTYP                         
003400*                                 TYPE OF ACCOUNTING                      
003500        05 TIAVIDAT          PIC S9(7)           COMP-3.                  
003600*                                 AVISERINGSDATUM (YYMMDD)                
003700*                                 ADVICE NOTE DATE                        
003800        05 IDHKTO            PIC 9(3).                                    
003900*                                 HUVUDKONTO                              
004000*                                 MAIN ACCOUNT                            
004100        05 IDUKTO            PIC 9(2).                                    
004200*                                 UNDERKONTO  PREL                        
004300*                                 SUB ACCOUNT                             
004400        05 IDKST             PIC 9(5).                                    
004500*                                 KOSTNADSSTÄLLE                          
004600*                                 COST CENTRE                             
004700        05 IDANALYSNR        PIC S9(9)           COMP-3.                  
004800*                                 ANALYSNUMMER                            
004900        05 IDFTG             PIC 9(2).                                    
005000*                                 FÖRETAGSID EKONOM REDOVISNING           
005100*                                 COMPANY IDENTITY ACCOUNTING             
005200        05 IDFS              PIC X(8).                                    
005300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
005400*                                 ADVICE NOTE NUMBER ODETTE               
005500        05 KVAVIS            PIC S9(7)           COMP-3.                  
005600*                                 AVISERAT ANTAL                          
005700*                                 QUANTITY NOTIFIED                       
005800        05 KDAVVANT          PIC S9              COMP-3.                  
005900*                                 AVVIKELSEANTAL KOD                      
006000*                                 0=INGEN ANM.   1=AVVIKELSE              
006100*                                 2=MAKULERING AV MOTT.RAPPORT            
006200*                                 QUANTITY DEVIATION  CODE                
006300*                                 0=NO DEV.    1=DEVIATION                
006400*                                 2=CANCELLING OF REC. REPORT             
006500        05 KVANTMOT          PIC S9(7)           COMP-3.                  
006600*                                 ANTAL MOTTAGET                          
006700*                                 QUANTITY RECEIVED                       
006800        05 KDAVVKV           PIC S9              COMP-3.                  
006900*                                 KVALITETSAVVIKELSEKOD                   
007000*                                 0=INGEN ANM.  1=AVVIKELSE               
007100*                                 2=AVVIKELSE, RETURNERAS                 
007200*                                 QUALITY DEVIATION CODE                  
007300*                                 0 = NO DEV.  1 = DEVIATION              
007400*                                 2 = DEVIATION, WILL BE RETURNED         
007500        05 KVRETUR           PIC S9(7)           COMP-3.                  
007600*                                 ANTAL I RETUR                           
007700*                                 QUANTITY IN RETURN                      
007800        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
007900*                                 LAGEROMRÅDE                             
008000*                                 AREA                                    
008100        05 BEFT              PIC S9(3)           COMP-3.                  
008200*                                 FÖRPACKNINGSTYP                         
008300*                                 PACKAGING TYPE                          
008400        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
008500*                                 ARTIKELSTANDARDPRIS                     
008600*                                 STANDARD PRICE                          
008700        05 KDFORP            PIC S9(5)           COMP-3.                  
008800*                                 FÖRPACKNINGSKOD                         
008900*                                 PACKAGING CODE                          
009000        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
009100*                                 SEPARAT PERIODBEHOV                     
009200*                                 SEPARATE PERIOD REQUIREMENTS            
009300        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
009400*                                 SATS-PERIODBEHOV                        
009500*                                 KIT PERIOD REQUIREMENTS                 
009600        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
009700*                                 EMBALLAGEKOD 0                          
009800        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
009900*                                 EMBALLAGEKOD 1                          
010000        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
010100*                                 EMBALLAGEKOD 2                          
010200        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
010300*                                 ANTAL I Q0 FÖRPACKNING                  
010400        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
010500*                                 ANTAL I Q1 FÖRPACKNING                  
010600*                                 QUANTITY IN BULK PACK Q1                
010700        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
010800*                                 ANTAL I Q2 FÖRPACKNING                  
010900*                                 QUANTITY IN BULK PACK Q2                
011000        05 KVKOLLI           PIC S9(5)           COMP-3.                  
011100*                                 ANTAL KOLLI                             
011200*                                 NBR OF CASES                            
011300        05 KVKOLLI-RAPP      PIC S9(5)           COMP-3.                  
011400*                                 ANTAL KOLLI                             
011500*                                 NBR OF CASES                            
011600        05 KVRAPP            PIC S9(7)           COMP-3.                  
011700*                                 DELRAPPORTERAT ANTAL                    
011800*                                 PARTIAL REPORTED QUANTITY               
011900*** END OF VILMAII-COPY LENGTH= 113 BYTES                                 
