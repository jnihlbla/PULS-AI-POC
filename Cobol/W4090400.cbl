000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4090400.                                                
000400 AUTHOR.         FRANK THORBURN                                           
000500 DATE-WRITTEN.   AUGUSTI 88.                                              
000600                                                                          
000700*    FUNKTION.                                                            
000800*    IMS-UPPDATERINGSPROGRAM FÖR REGISTRERING AV ORDERHUVUDEN             
000900*    (FRISLÄPPNING AV RESTORDER.)                                         
001000*                                                                         
001100*    PROGRAMMET UPPDATERAR       WDB4                                     
001200                                                                          
001300                                                                          
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T904                                              
001600*        MID:         W4I90401                                            
001700                                                                          
001800*    UTDATA.                                                              
001900*        MOD:         W4O90401                                            
002000*                                                                         
002100* CHANGE LOG:                                                             
002200*                                                                         
002300*                                                                         
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000*    -COPY WY2000W1                                                       
003100     SKIP3                                                                
003200 77   PROGRAM-NAMN           VALUE 'W4090400'                             
003300                                 PIC X(8).                                
003400 77  JA                          PIC X(1)   VALUE 'J'.                    
003500 77  NEJ                         PIC X(1)   VALUE 'N'.                    
003510 77  W-IDFTG-B6                  PIC 9(2)   VALUE ZERO.                   
003600 77  SPRAK-IX                    PIC S9(9)  VALUE +0   COMP SYNC.         
003700 77  IX                          PIC S9(9)  VALUE +0   COMP SYNC.         
003800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +430 COMP SYNC.         
003900 77  WS-IDDISTR                  PIC X(4)   VALUE ZERO.                   
004000 77  WS-IDKUNDNR                 PIC X(6)   VALUE ZERO.                   
004100 77  WS-KDFRAKT                  PIC X(2)   VALUE ZERO.                   
004200 77  WS-KDORDKL                  PIC X(1)   VALUE ZERO.                   
004300 77  WS-FLAENDR                  PIC X(1)   VALUE SPACE.                  
004400 77  SW-INDATA-OK                PIC X(1).                                
004500     88  INDATA-OK                          VALUE 'J'.                    
004600 77  SW-NYA-NYCKLAR              PIC X(1).                                
004700     88  NYA-NYCKLAR                        VALUE 'J'.                    
004800 77  SW-NYCKLAR-OK               PIC X(1).                                
004900     88  NYCKLAR-OK                         VALUE 'J'.                    
005000 77  SW-TID-IFYLLD               PIC X(1).                                
005100     88  TID-IFYLLD                         VALUE 'J'.                    
005200 77  WS-IDTRANS                  PIC X(4).                                
005300     88  EGEN-BILD                          VALUE '4904'.                 
005400     88  WS-GODKAEND-BILD                   VALUE '4903' '4904'.          
005500 77  SW-DATA-IFYLLD              PIC X(1).                                
005600     88  DATA-IFYLLD                        VALUE 'J'.                    
005700                                                                          
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100*                                                                         
006200 01  GEMENSAMMA-SUBPROGRAM.                                               
006300*                                                                         
006400     03  W411SAP                 PIC X(8)    VALUE 'W411SAP '.            
006500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006600*            SAP KONTROLL                                                 
006700*                                                                         
006800*     ----------  SUBPROGRAM  --------------                              
006900*                                                                         
007000 01  FILLER                      PIC  X(12) VALUE                         
007100                                               'SAP KONTROLL'.            
007200*                                                                         
007300*01 -COPY W411SAP                                                         
007400     EJECT                                                                
007500*01  -COPY WWIDFTG                                                        
007600     EJECT                                                                
007700*                                                                         
007800 01  SPRAK-TYP                   PIC X(3).                                
007900     88  GODK-SPRAK          VALUE 'S  ' 'GB ' 'D  ' 'F  ' 'E  '.         
008000 01  BIPACK-TYP                  PIC X(1).                                
008100     88  GODK-BIPACKKOD      VALUE '1' '2' '3' '4' '5' '6' '7'            
008200                           '8' ' ' 'A' 'B' 'C' 'D' 'E' 'F' 'G'            
008300                           'H' 'I' 'J' 'K' 'M'.                           
008400     88  SPEC-BIPACKKOD      VALUE '5' '6' '8'.                           
008500 01  FAKTURA-TYP                 PIC X(1).                                
008600     88  GODK-FAKTURATYP     VALUE 'R' 'G' 'K' 'N'.                       
008700     88  GODK-FAKTYP-KONTO   VALUE 'G' 'N'.                               
008800                                                                          
008900     EJECT                                                                
009000 01  NYCKLAR-TILL-DLI.                                                    
009100                                                                          
009200     03  W-WDB401KEY-X.                                                   
009300       05  W-IDDISTR             PIC S9(5) VALUE ZERO  COMP-3.            
009400       05  W-IDKUNDNR            PIC S9(7) VALUE ZERO  COMP-3.            
009500                                                                          
009600     03  W-WDB414KEY-X.                                                   
009700       05  W-KDFRAKT             PIC S9(3) VALUE ZERO  COMP-3.            
009800       05  W-KDORDKL             PIC S9(1) VALUE ZERO  COMP-3.            
009900                                                                          
010000     03  W-WDB201KEY-X.                                                   
010100       05  W-WDB2-IDDISTR        PIC S9(5) VALUE ZERO  COMP-3.            
010200       05  W-WDB2-IDKUNDNR       PIC S9(7) VALUE ZERO  COMP-3.            
010300                                                                          
010500 01  ARBETS-FALT.                                                         
010600     03  ARB-TISTADAT            PIC Z(6).                                
010700     03  ARB-DATUM               PIC 9(6).                                
010800     03  ARB-KONTO               PIC X(11)  VALUE ZERO.                   
010900     03  ARB-KONTO-X   REDEFINES ARB-KONTO.                               
011000         05 ARB-IDKONTO-1        PIC X(1).                                
011100         05 ARB-IDKONTO-2        PIC X(10).                               
011200     03  ARB-IDKST               PIC X(10)  VALUE SPACE.                  
011300     03  ARB-IDANALYS            PIC X(12)  VALUE ZERO.                   
011400     03  ARB-IDFTG               PIC 9(2)   VALUE ZERO.                   
011500                                                                          
011600 01  W-SPAR-AREA                 PIC X(26)  VALUE SPACES.                 
011700                                                                          
011800 EJECT                                                                    
011900 01  SPRAK-TEXTER.                                                        
012000                                                                          
012100     03  SVENSK.                                                          
012200         05  FILLER  PIC X(8)    VALUE 'SVENSKA '.                        
012300         05  FILLER  PIC X(8)    VALUE 'SWEDISH '.                        
012400     03  FILLER REDEFINES SVENSK.                                         
012500         05  SVENSKA OCCURS 2    PIC X(8).                                
012600     03  ENGELSK.                                                         
012700         05  FILLER  PIC X(8)    VALUE 'ENGELSKA'.                        
012800         05  FILLER  PIC X(8)    VALUE 'ENGLISH '.                        
012900     03  FILLER REDEFINES ENGELSK.                                        
013000         05  ENGELSKA OCCURS 2   PIC X(8).                                
013100     03  TYSK.                                                            
013200         05  FILLER  PIC X(8)    VALUE 'TYSKA   '.                        
013300         05  FILLER  PIC X(8)    VALUE 'GERMAN  '.                        
013400     03  FILLER REDEFINES TYSK.                                           
013500         05  TYSKA OCCURS 2      PIC X(8).                                
013600     03  FRANSK.                                                          
013700         05  FILLER  PIC X(8)    VALUE 'FRANSKA '.                        
013800         05  FILLER  PIC X(8)    VALUE 'FRENCH  '.                        
013900     03  FILLER REDEFINES FRANSK.                                         
014000         05  FRANSKA OCCURS 2    PIC X(8).                                
014100     03  SPANSK.                                                          
014200         05  FILLER  PIC X(8)    VALUE 'SPANSKA '.                        
014300         05  FILLER  PIC X(8)    VALUE 'SPANISH '.                        
014400     03  FILLER REDEFINES SPANSK.                                         
014500         05  SPANSKA OCCURS 2    PIC X(8).                                
014600                                                                          
014700     EJECT                                                                
014800 01  FAKTURA-TEXTER.                                                      
014900                                                                          
015000     03  EXTERNFAK.                                                       
015100         05  FILLER  PIC X(20)   VALUE 'EXTERN FAKTURA      '.            
015200         05  FILLER  PIC X(20)   VALUE 'EXTERNAL INVOICE    '.            
015300     03  FILLER REDEFINES EXTERNFAK.                                      
015400         05  EXTERN OCCURS 2     PIC X(20).                               
015500     03  GRATISFAK.                                                       
015600         05  FILLER  PIC X(20)   VALUE 'GRATISFAKTURA       '.            
015700         05  FILLER  PIC X(20)   VALUE 'FREE OF CHARGE      '.            
015800     03  FILLER REDEFINES GRATISFAK.                                      
015900         05  GRATIS OCCURS 2     PIC X(20).                               
016000     03  KONSIGNATIONSFAK.                                                
016100         05  FILLER  PIC X(20)   VALUE 'KONSIGNATIONSFAKTURA'.            
016200         05  FILLER  PIC X(20)   VALUE 'CONSIGNMENT INVOICE '.            
016300     03  FILLER REDEFINES KONSIGNATIONSFAK.                               
016400         05  KONSIGNATION OCCURS 2 PIC X(20).                             
016500     03  INTERNFAK.                                                       
016600         05  FILLER  PIC X(20)   VALUE 'INTERN DEBITERING   '.            
016700         05  FILLER  PIC X(20)   VALUE 'INTERNAL DEBET NOTE '.            
016800     03  FILLER REDEFINES INTERNFAK.                                      
016900         05  INTERN OCCURS 2     PIC X(20).                               
017000                                                                          
017100     EJECT                                                                
017200 01  BO-RELEASE-TEXTER.                                                   
017300                                                                          
017400     03  KXROPACK0.                                                       
017500         05  FILLER  PIC X(20)   VALUE 'INGEN BIPACKNING    '.            
017600         05  FILLER  PIC X(20)   VALUE 'NO CONSOLIDATION    '.            
017700     03  FILLER REDEFINES KXROPACK0.                                      
017800         05  KXROPACK-0 OCCURS 2     PIC X(20).                           
017900                                                                          
018000     03  KXROPACK1.                                                       
018100         05  FILLER  PIC X(20)   VALUE 'SAMTLIGA RO-TPO     '.            
018200         05  FILLER  PIC X(20)   VALUE 'ALL BO/TPO          '.            
018300     03  FILLER REDEFINES KXROPACK1.                                      
018400         05  KXROPACK-1 OCCURS 2     PIC X(20).                           
018500                                                                          
018600     03  KXROPACK2.                                                       
018700         05  FILLER  PIC X(20)   VALUE 'SAMMA ORDERKLASS    '.            
018800         05  FILLER  PIC X(20)   VALUE 'SAME ORDER CLASS    '.            
018900     03  FILLER REDEFINES KXROPACK2.                                      
019000         05  KXROPACK-2 OCCURS 2     PIC X(20).                           
019100                                                                          
019200     03  KXROPACK3.                                                       
019300         05  FILLER  PIC X(20)   VALUE 'SAMMA EL LÄGRE KLASS'.            
019400         05  FILLER  PIC X(20)   VALUE 'SAME OR LOWER CLASS '.            
019500     03  FILLER REDEFINES KXROPACK3.                                      
019600         05  KXROPACK-3 OCCURS 2     PIC X(20).                           
019700                                                                          
019800     03  KXROPACK4.                                                       
019900         05  FILLER  PIC X(20)   VALUE 'SAMMA FRAKTKOD      '.            
020000         05  FILLER  PIC X(20)   VALUE 'SAME FREIGHT CODE   '.            
020100     03  FILLER REDEFINES KXROPACK4.                                      
020200         05  KXROPACK-4 OCCURS 2     PIC X(20).                           
020300                                                                          
020400     03  KXROPACK5.                                                       
020500         05  FILLER  PIC X(20)   VALUE 'UTPEKANDE ORDERNR.  '.            
020600         05  FILLER  PIC X(20)   VALUE 'SPECIFIED ORDER.NO. '.            
020700     03  FILLER REDEFINES KXROPACK5.                                      
020800         05  KXROPACK-5 OCCURS 2     PIC X(20).                           
020900                                                                          
021000     03  KXROPACK6.                                                       
021100         05  FILLER  PIC X(20)   VALUE 'UDDA EL JÄMNA FR KOD'.            
021200         05  FILLER  PIC X(20)   VALUE 'ODD OR EVEN FR.CODES'.            
021300     03  FILLER REDEFINES KXROPACK6.                                      
021400         05  KXROPACK-6 OCCURS 2     PIC X(20).                           
021500                                                                          
021600     03  KXROPACK7.                                                       
021700         05  FILLER  PIC X(20)   VALUE 'BYTES               '.            
021800         05  FILLER  PIC X(20)   VALUE 'EXCHANGE            '.            
021900     03  FILLER REDEFINES KXROPACK7.                                      
022000         05  KXROPACK-7 OCCURS 2     PIC X(20).                           
022100                                                                          
022200     03  KXROPACK8.                                                       
022300         05  FILLER  PIC X(20)   VALUE 'EJ UTPEKADE ORDERNR.'.            
022400         05  FILLER  PIC X(20)   VALUE 'NOT SPECIFIED ORDERS'.            
022500     03  FILLER REDEFINES KXROPACK8.                                      
022600         05  KXROPACK-8 OCCURS 2     PIC X(20).                           
022700                                                                          
022800     03  KXROPACKA.                                                       
022900         05  FILLER  PIC X(20)   VALUE 'SAMTL. KL 3 OCH 4   '.            
023000         05  FILLER  PIC X(20)   VALUE 'ALL CLASS 3 AND 4   '.            
023100     03  FILLER REDEFINES KXROPACKA.                                      
023200         05  KXROPACK-A OCCURS 2     PIC X(20).                           
023300                                                                          
023400     03  KXROPACKB.                                                       
023500         05  FILLER  PIC X(20)   VALUE 'SAMTL KL 2, 3 OCH 4 '.            
023600         05  FILLER  PIC X(20)   VALUE 'ALL CL. 2, 3 AND 4  '.            
023700     03  FILLER REDEFINES KXROPACKB.                                      
023800         05  KXROPACK-B OCCURS 2     PIC X(20).                           
023900                                                                          
024000     03  KXROPACKC.                                                       
024100         05  FILLER  PIC X(20)   VALUE 'SAMTL KL 1          '.            
024200         05  FILLER  PIC X(20)   VALUE 'ALL CL. 1           '.            
024300     03  FILLER REDEFINES KXROPACKC.                                      
024400         05  KXROPACK-C OCCURS 2     PIC X(20).                           
024500                                                                          
024600     03  KXROPACKD.                                                       
024700         05  FILLER  PIC X(20)   VALUE 'SAMTL KL 1 OCH 2    '.            
024800         05  FILLER  PIC X(20)   VALUE 'ALL CL. 1 AND 2     '.            
024900     03  FILLER REDEFINES KXROPACKD.                                      
025000         05  KXROPACK-D OCCURS 2     PIC X(20).                           
025100                                                                          
025200     03  KXROPACKE.                                                       
025300         05  FILLER  PIC X(20)   VALUE 'SAMTLIGA TPO        '.            
025400         05  FILLER  PIC X(20)   VALUE 'ALL TPO             '.            
025500     03  FILLER REDEFINES KXROPACKE.                                      
025600         05  KXROPACK-E OCCURS 2     PIC X(20).                           
025700                                                                          
025800     03  KXROPACKF.                                                       
025900         05  FILLER  PIC X(20)   VALUE 'SAMMA KTO/KST       '.            
026000         05  FILLER  PIC X(20)   VALUE 'SAME ACCOUNT/C.C    '.            
026100     03  FILLER REDEFINES KXROPACKF.                                      
026200         05  KXROPACK-F OCCURS 2     PIC X(20).                           
026300                                                                          
026400     03  KXROPACKG.                                                       
026500         05  FILLER  PIC X(20)   VALUE 'SAMMA ORDERKLASS/FK '.            
026600         05  FILLER  PIC X(20)   VALUE 'SAME ORDER CL/FR.CD '.            
026700     03  FILLER REDEFINES KXROPACKG.                                      
026800         05  KXROPACK-G OCCURS 2     PIC X(20).                           
026900                                                                          
027000     03  KXROPACKH.                                                       
027100         05  FILLER  PIC X(20)   VALUE 'ENDAST RO           '.            
027200         05  FILLER  PIC X(20)   VALUE 'ONLY BO             '.            
027300     03  FILLER REDEFINES KXROPACKH.                                      
027400         05  KXROPACK-H OCCURS 2     PIC X(20).                           
027500                                                                          
027600     03  KXROPACKI.                                                       
027700         05  FILLER  PIC X(20)   VALUE 'ENDAST RO/SAMMA FK  '.            
027800         05  FILLER  PIC X(20)   VALUE 'ONLY BO/SAME FR.CD  '.            
027900     03  FILLER REDEFINES KXROPACKI.                                      
028000         05  KXROPACK-I OCCURS 2     PIC X(20).                           
028100                                                                          
028200     03  KXROPACKJ.                                                       
028300         05  FILLER  PIC X(20)   VALUE 'RO KL 3 OCH 4       '.            
028400         05  FILLER  PIC X(20)   VALUE 'BO CL 3 AND 4       '.            
028500     03  FILLER REDEFINES KXROPACKJ.                                      
028600         05  KXROPACK-J OCCURS 2     PIC X(20).                           
028700                                                                          
028800     03  KXROPACKK.                                                       
028900         05  FILLER  PIC X(20)   VALUE 'RO KL 2,3 OCH 4     '.            
029000         05  FILLER  PIC X(20)   VALUE 'BO KL 2,3 AND 4     '.            
029100     03  FILLER REDEFINES KXROPACKK.                                      
029200         05  KXROPACK-K OCCURS 2     PIC X(20).                           
029300                                                                          
029400     03  KXROPACKM.                                                       
029500         05  FILLER  PIC X(20)   VALUE 'TPO-3               '.            
029600         05  FILLER  PIC X(20)   VALUE 'TPO-3               '.            
029700     03  FILLER REDEFINES KXROPACKM.                                      
029800         05  KXROPACK-M OCCURS 2     PIC X(20).                           
029900     EJECT                                                                
030000*01  -COPY WWTEXT01.                                                      
030100     EJECT                                                                
030200 01  TEST-IDDISTR    PIC 9(5) COMP-3.                                     
030300     EJECT                                                                
030400*01  FILLER -COPY WWDIST20 -RED TEST-IDDISTR.                             
030500     EJECT                                                                
030600                                                                          
030700 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
030800     SKIP3                                                                
030900 01    FILLER              PIC X(16)   VALUE 'MID W4I904 MID'.            
031000*01  MID -COPY W4I90401.                                                  
031100     EJECT                                                                
031200                                                                          
031300 01  FILLER                PIC X(16)   VALUE 'WMSGINIT'.                  
031400                                                                          
031500*01  -COPY WMSGINIT                                                       
031600                                                                          
031700*01  -COPY WMSGAREA                                                       
031800     EJECT                                                                
031900*    03  MOD -COPY W4O90401  -RED MSG-AREA.                               
032000     EJECT                                                                
032100*01  -COPY WMFSAREA                                                       
032200     EJECT                                                                
032300 01  IMS-WS.                                                              
032400     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
032500                                                                          
032600*                        **** STATUS-KOD FRÅN IMS                         
032700     03  STATUS-WS               PIC X(2).                                
032800         88  SEGMENT-FINNS                   VALUE '  '.                  
032900         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
033000                                                                          
033100     03  GODK-STATUSKODER.                                                
033200         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
033300                                                                          
033400 01  SSA1                        PIC X(64).                               
033500 01  SSA2                        PIC X(64).                               
033600     EJECT                                                                
033700*                            IMS FUNKTIONSKODER                           
033800*01  -COPY W0003                                                          
033900     EJECT                                                                
034000*                            DLI INPUT-OUTPUT AREA                        
034100 01  DLI-IO-AREA.                                                         
034200     03  IO-AREA                 PIC X(600)  VALUE SPACE.                 
034300                                                                          
034400*    03  WLKNDD01 -COPY WDB401                -RED IO-AREA.               
034500                                                                          
034600     EJECT                                                                
034700*    03  WLKNDD14 -COPY WDB414                -RED IO-AREA.               
034800                                                                          
034900     EJECT                                                                
035000 01  DLI-IO-AREA-WDB2.                                                    
035100*    03  WLGMTA01 -COPY WDB201                                            
035200                                                                          
035210 01  FILLER                      PIC X(20)  VALUE                         
035220                                          'DLI-IO-AREA-WDB601'.           
035230 01  DLI-IO-AREA-WDB601.                                                  
035240     03  WDB601.                                                          
035250*        05  -COPY WDB601                                                 
035300     EJECT                                                                
035400 LINKAGE SECTION.                                                         
035500*01  -COPY W0009     -PRE MSG-                                            
035600     EJECT                                                                
035700*01  -COPY W0008     -PRE USEA-                                           
035800     05  FILLER                  PIC X.                                   
035900     EJECT                                                                
036000*01  -COPY W0008     -PRE WDB4-                                           
036100     05  FILLER                  PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008     -PRE WDB2-                                           
036400     05  FILLER                  PIC X.                                   
036500*01  -COPY W0008     -PRE WDH3-                                           
036600     05  FILLER                  PIC X.                                   
036610*01  -COPY W0008     -PRE WDB6-                                           
036620     05  FILLER                  PIC X.                                   
036700     EJECT                                                                
036800 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
036900                          WDB4-PCB WDB2-PCB WDH3-PCB WDB6-PCB.            
037000                                                                          
037100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
037200                           WDB4-PCB WDB2-PCB WDH3-PCB WDB6-PCB.           
037300                                                                          
037400                                                                          
037500     PERFORM IMS-GET-MSG                                                  
037600     IF SEGMENT-FINNS                                                     
037700       PERFORM A-INIT                                                     
037800       IF WS-GODKAEND-BILD                                                
037900                                                                          
038000         PERFORM B-KOLLA-NYCKLAR                                          
038100         IF NYCKLAR-OK                                                    
038200           IF MFS-UPDATE                                                  
038300             IF NYA-NYCKLAR                                               
038400               MOVE TEXT-0401 (SPRAK-IX) TO MOD-TEMFSFEL                  
038500*              FEL NYCKEL                                                 
038600             ELSE                                                         
038700               MOVE WS-IDDISTR                TO W-IDDISTR                
038800               MOVE WS-IDKUNDNR               TO W-IDKUNDNR               
038900               PERFORM IMS-GU-WDB401                                      
039000               IF SEGMENT-SAKNAS                                          
039100                 MOVE WS-IDDISTR              TO W-WDB2-IDDISTR           
039200                 MOVE WS-IDKUNDNR             TO W-WDB2-IDKUNDNR          
039300                 PERFORM IMS-GU-WDB201                                    
039400                 IF SEGMENT-SAKNAS                                        
039500                   MOVE TEXT-0412 (SPRAK-IX)  TO MOD-TEMFSFEL             
039600*                  DISTRIKT/KUND SAKNAS PÅ KUNDREGISTRET                  
039700                 ELSE                                                     
039800                   MOVE WS-IDDISTR   TO  W-IDDISTR                        
039900                                         KUND-IDDISTR                     
040000                   MOVE WS-IDKUNDNR  TO  W-IDKUNDNR                       
040100                                         KUND-IDKUNDNR                    
040200                                                                          
040300                   PERFORM IMS-INSERT-WDB401                              
040400                   PERFORM IMS-GU-WDB401                                  
040500                   PERFORM D-KOLLA-O-UPPDATERA                            
040600                 END-IF                                                   
040700               ELSE                                                       
040800                 PERFORM D-KOLLA-O-UPPDATERA                              
040900               END-IF                                                     
041000             END-IF                                                       
041100           ELSE                                                           
041200             PERFORM C-KOLLA-INDATA-IFYLLD                                
041300             IF DATA-IFYLLD                                               
041400               MOVE TEXT-0407 (SPRAK-IX)    TO MOD-TEMFSFEL               
041500*              TRYCK PF11 FÖR UPPDATERING                                 
041600               PERFORM MFS-ROER-EJ-FALT                                   
041700             END-IF                                                       
041800             IF NYA-NYCKLAR                                               
041900               IF NOT DATA-IFYLLD                                         
042000                 PERFORM MFS-RENSA-BILD                                   
042100               END-IF                                                     
042200               PERFORM E-LAS-WDB401                                       
042300             ELSE                                                         
042400               IF NOT DATA-IFYLLD                                         
042500                 PERFORM E-LAS-WDB401                                     
042600               END-IF                                                     
042700             END-IF                                                       
042800           END-IF                                                         
042900         ELSE                                                             
043000           MOVE TEXT-0401 (SPRAK-IX)          TO MOD-TEMFSFEL             
043100*          FEL NYCKEL                                                     
043200         END-IF                                                           
043300       ELSE                                                               
043400         PERFORM MFS-RENSA-NYCKLAR                                        
043500       END-IF                                                             
043600       PERFORM IMS-INSERT-MSG                                             
043700     END-IF                                                               
043800     MOVE ZERO TO RETURN-CODE                                             
043900     GOBACK                                                               
044000                                                                          
044100     .                                                                    
044200     EJECT                                                                
044300 A-INIT SECTION.                                                          
044400                                                                          
044500     IF MSG-DUBBLA-TRANSKODER                                             
044600         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I90401-CTX           
044700         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
044800         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
044900         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
045000         MOVE MSG-IDPFK TO MFS-IDPFK                                      
045100     ELSE                                                                 
045200         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I90401-CTX            
045300         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
045400         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
045500         MOVE SPACE TO MFS-KDTRTYP        MFS-IDPFK                       
045600     END-IF                                                               
045700     MOVE MFS-IDTRANS                  TO WS-IDTRANS                      
045800                                                                          
045900     MOVE LOW-VALUE                    TO MSG-AREA                        
046000     MOVE 'W4O90401'                   TO MFS-IDMOD                       
046100     MOVE '4904'                       TO MOD-IDTRANS                     
046200     MOVE MAX-MOD-LAENGD               TO MSG-KVLL                        
046300                                                                          
046400     MOVE NEJ                          TO SW-DATA-IFYLLD                  
046500                                                                          
046600     PERFORM MFS-RENSA-BILD                                               
046700     PERFORM MFS-RENSA-NYCKLAR                                            
046800                                                                          
046900     IF NOT EGEN-BILD                                                     
047000         MOVE SPACE                    TO MFS-KDTRTYP                     
047100         MOVE '7'                      TO MFS-IDPFK                       
047200     END-IF                                                               
047300                                                                          
047400     IF ENGLISH-TEXT                                                      
047500         MOVE +2                       TO SPRAK-IX                        
047600     ELSE                                                                 
047700         MOVE +1                       TO SPRAK-IX                        
047800     END-IF                                                               
047900                                                                          
048000     MOVE ALL '+'                      TO MSGI-WMSGINIT                   
048100     MOVE '001'                        TO MSGI-KDCALL                     
048200     MOVE MSG-SIGNON-USERID            TO MSGI-IDUSER                     
048300     MOVE '4904'                       TO MSGI-IDTRANS                    
048400     MOVE MSG-LTERM-NAME               TO MSGI-IDLTERM-USER               
048500                                                                          
048600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB.                          
048700                                                                          
048800     EJECT                                                                
048900 B-KOLLA-NYCKLAR SECTION.                                                 
049000                                                                          
049100     MOVE NEJ                          TO SW-NYA-NYCKLAR                  
049200                                                                          
049300     IF MID-IDDISTR-IN = ALL '+'                                          
049400       MOVE MID-IDDISTR-UT             TO WS-IDDISTR                      
049500       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
049600     ELSE                                                                 
049700       MOVE MID-IDDISTR-IN             TO WS-IDDISTR                      
049800       MOVE JA                         TO SW-NYA-NYCKLAR                  
049900       MOVE '7'                        TO MFS-IDPFK                       
050000     END-IF                                                               
050100                                                                          
050200     MOVE WS-IDDISTR                   TO MOD-IDDISTR-UT                  
050300                                          TEST-IDDISTR                    
050400     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
050500                                                                          
050600     IF MID-IDKUNDNR-IN = ALL '+'                                         
050700       MOVE MID-IDKUNDNR-UT            TO WS-IDKUNDNR                     
050800       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
050900     ELSE                                                                 
051000       MOVE MID-IDKUNDNR-IN            TO WS-IDKUNDNR                     
051100       MOVE JA                         TO SW-NYA-NYCKLAR                  
051200       MOVE '7'                        TO MFS-IDPFK                       
051300     END-IF                                                               
051400                                                                          
051500     MOVE WS-IDKUNDNR                  TO MOD-IDKUNDNR-UT                 
051600     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
051700                                                                          
051800     IF MID-KDFRAKT-IN = ALL '+'                                          
051900       MOVE MID-KDFRAKT-UT             TO WS-KDFRAKT                      
052000       INSPECT WS-KDFRAKT                                                 
052100                  REPLACING LEADING SPACE BY ZERO                         
052200     ELSE                                                                 
052300       MOVE MID-KDFRAKT-IN             TO WS-KDFRAKT                      
052400       MOVE JA                         TO SW-NYA-NYCKLAR                  
052500       MOVE '7'                        TO MFS-IDPFK                       
052600     END-IF                                                               
052700                                                                          
052800     MOVE WS-KDFRAKT TO MOD-KDFRAKT-UT                                    
052900     INSPECT MOD-KDFRAKT-UT REPLACING LEADING ZERO BY SPACE               
053000                                                                          
053100                                                                          
053200     IF MID-KDORDKL-IN = ALL '+'                                          
053300       MOVE MID-KDORDKL-UT             TO WS-KDORDKL                      
053400       INSPECT WS-KDORDKL                                                 
053500                  REPLACING LEADING SPACE BY ZERO                         
053600     ELSE                                                                 
053700       MOVE MID-KDORDKL-IN             TO WS-KDORDKL                      
053800       MOVE JA                         TO SW-NYA-NYCKLAR                  
053900       MOVE '7'                        TO MFS-IDPFK                       
054000     END-IF                                                               
054100                                                                          
054200     MOVE WS-KDORDKL TO MOD-KDORDKL-UT                                    
054300     INSPECT MOD-KDORDKL-UT REPLACING LEADING ZERO BY SPACE               
054400                                                                          
054500     IF EGEN-BILD                                                         
054600       IF MID-FLAENDR-IN = ALL '+'                                        
054700         MOVE MID-FLAENDR-UT             TO WS-FLAENDR                    
054800         INSPECT WS-FLAENDR                                               
054900                    REPLACING LEADING SPACE BY ZERO                       
055000       ELSE                                                               
055100         MOVE MID-FLAENDR-IN             TO WS-FLAENDR                    
055200       END-IF                                                             
055300                                                                          
055400       MOVE WS-FLAENDR TO MOD-FLAENDR-UT                                  
055500       INSPECT MOD-FLAENDR-UT REPLACING LEADING ZERO BY SPACE             
055600     ELSE                                                                 
055700       MOVE MFS-RENSA-FAELT              TO MOD-FLAENDR-UT                
055800     END-IF                                                               
055900                                                                          
056000     IF WS-IDTRANS = '4903'                                               
056100       MOVE MID-W4I90401-CTX(1:26)       TO W-SPAR-AREA                   
056200       MOVE ALL '+'                      TO MID-W4I90401-CTX              
056300       MOVE W-SPAR-AREA(1:26)            TO MID-W4I90401-CTX              
056400       MOVE JA                           TO SW-NYA-NYCKLAR                
056500     END-IF                                                               
056600                                                                          
056700     PERFORM BA-KONTROLL-INPUT                                            
056800                                                                          
056900     .                                                                    
057000     EJECT                                                                
057100 BA-KONTROLL-INPUT SECTION.                                               
057200                                                                          
057300     IF (WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO) AND                    
057400        (WS-KDFRAKT NUMERIC AND WS-KDFRAKT > ZERO) AND                    
057500        (WS-KDORDKL NUMERIC) AND                                          
057600         WS-IDKUNDNR               NUMERIC                                
057700       IF (WS-KDORDKL = 1 OR 2 OR 3 OR 4)                                 
057800         MOVE JA                       TO SW-NYCKLAR-OK                   
057900       ELSE                                                               
058000         MOVE NEJ                    TO SW-NYCKLAR-OK                     
058100       END-IF                                                             
058200     ELSE                                                                 
058300       MOVE NEJ                    TO SW-NYCKLAR-OK                       
058400     END-IF                                                               
058500     .                                                                    
058600     EJECT                                                                
058700 C-KOLLA-INDATA-IFYLLD SECTION.                                           
058800                                                                          
058900     IF WS-FLAENDR = 'J' OR 'Y'                                           
059000       MOVE JA                       TO SW-DATA-IFYLLD                    
059100     ELSE                                                                 
059200       MOVE MFS-RENSA-FAELT          TO MOD-FLAENDR-UT                    
059300     END-IF                                                               
059400                                                                          
059500     IF MID-BEKUNDRF-001 NOT = ALL '+'                                    
059600       MOVE MID-BEKUNDRF-001         TO MOD-BEKUNDRF-001                  
059700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-BEKUNDRF-ATTR                 
059800       MOVE JA                       TO SW-DATA-IFYLLD                    
059900     ELSE                                                                 
060000       MOVE MFS-ROER-EJ-FAELT        TO MOD-BEKUNDRF-001                  
060100     END-IF                                                               
060200                                                                          
060300     IF MID-IDSKYLT NOT = ALL '+'                                         
060400       MOVE MID-IDSKYLT              TO MOD-IDSKYLT                       
060500       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-IDSKYLT-ATTR                  
060600                                        MOD-KXSPRAK-ATTR                  
060700       MOVE JA                       TO SW-DATA-IFYLLD                    
060800     ELSE                                                                 
060900       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDSKYLT                       
061000                                        MOD-KXSPRAK                       
061100     END-IF                                                               
061200                                                                          
061300     IF MID-BEVARREF NOT = ALL '+'                                        
061400       MOVE MID-BEVARREF             TO MOD-BEVARREF                      
061500       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-BEVARREF-ATTR                 
061600       MOVE JA                       TO SW-DATA-IFYLLD                    
061700     ELSE                                                                 
061800       MOVE MFS-ROER-EJ-FAELT        TO MOD-BEVARREF                      
061900     END-IF                                                               
062000                                                                          
062100     IF MID-KDROPACK NOT = ALL '+'                                        
062200       MOVE MID-KDROPACK             TO MOD-KDROPACK                      
062300       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-KDROPACK-ATTR                 
062400                                        MOD-KXROPACK-ATTR                 
062500       MOVE JA                       TO SW-DATA-IFYLLD                    
062600     ELSE                                                                 
062700       MOVE MFS-ROER-EJ-FAELT        TO MOD-KDROPACK                      
062800                                        MOD-KXROPACK                      
062900     END-IF                                                               
063000                                                                          
063100     IF MID-KDFAKTYP NOT = '+'                                            
063200       MOVE MID-KDFAKTYP             TO MOD-KDFAKTYP                      
063300       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-KDFAKTYP-ATTR                 
063400                                        MOD-KXFAKTYP-ATTR                 
063500       MOVE JA                       TO SW-DATA-IFYLLD                    
063600     ELSE                                                                 
063700       MOVE MFS-ROER-EJ-FAELT        TO MOD-KDFAKTYP                      
063800                                        MOD-KXFAKTYP                      
063900     END-IF                                                               
064000                                                                          
064100     IF MID-IDKONTO-IN NOT = ALL '+'                                      
064200       MOVE MID-IDKONTO-IN         TO MOD-IDKONTO-IN                      
064300       INSPECT MOD-IDKONTO-IN REPLACING LEADING ZERO BY SPACE             
064400       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDKONTO-IN-ATTR                 
064500       MOVE JA                     TO SW-DATA-IFYLLD                      
064600     ELSE                                                                 
064700       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKONTO-UT                      
064800     END-IF                                                               
064900                                                                          
065000     IF MID-IDKST-IN NOT = ALL '+'                                        
065100       MOVE MID-IDKST-IN           TO MOD-IDKST-IN                        
065200       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDKST-IN-ATTR                   
065300       MOVE JA                     TO SW-DATA-IFYLLD                      
065400     ELSE                                                                 
065500       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKST-UT                        
065600     END-IF                                                               
065700                                                                          
065800     IF MID-IDFTG-IN NOT = ALL '+'                                        
065900       MOVE MID-IDFTG-IN           TO MOD-IDFTG-IN                        
066000       INSPECT MOD-IDFTG-IN REPLACING LEADING ZERO BY SPACE               
066100       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDFTG-IN-ATTR                   
066200       MOVE JA                     TO SW-DATA-IFYLLD                      
066300     ELSE                                                                 
066400       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDFTG-UT                        
066500     END-IF                                                               
066600                                                                          
066700     IF MID-IDANALYS-IN NOT = ALL '+'                                     
066800       MOVE MID-IDANALYS-IN        TO MOD-IDANALYS-IN                     
066900       INSPECT MOD-IDANALYS-IN REPLACING LEADING ZERO BY SPACE            
067000       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDANALYS-IN-ATTR                
067100       MOVE JA                     TO SW-DATA-IFYLLD                      
067200     ELSE                                                                 
067300       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDANALYS-UT                     
067400     END-IF                                                               
067500                                                                          
067600     IF MID-KDNOTES NOT = ALL '+'                                         
067700       MOVE MID-KDNOTES              TO MOD-KDNOTES                       
067800       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-KDNOTES-ATTR                  
067900       MOVE JA                       TO SW-DATA-IFYLLD                    
068000     ELSE                                                                 
068100       MOVE MFS-ROER-EJ-FAELT        TO MOD-KDNOTES                       
068200     END-IF                                                               
068300                                                                          
068400     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10                         
068500       IF MID-TID (IX) NOT = '+'                                          
068600         MOVE MID-TID (IX)           TO MOD-TID (IX)                      
068700         MOVE MFS-ROER-EJ-FAELT      TO MOD-TID (IX)                      
068800         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TID-ATTR (IX)                 
068900         MOVE JA                     TO SW-DATA-IFYLLD                    
069000       ELSE                                                               
069100         MOVE MFS-ROER-EJ-FAELT      TO MOD-TID (IX)                      
069200       END-IF                                                             
069300     END-PERFORM                                                          
069400                                                                          
069500     IF MID-TISTADAT NOT = ALL '+'                                        
069600       MOVE MID-TISTADAT           TO MOD-TISTADAT                        
069700*      MOVE MFS-ROER-EJ-FAELT      TO MOD-TISTADAT                        
069800       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TISTADAT-ATTR                   
069900       MOVE JA                       TO SW-DATA-IFYLLD                    
070000     ELSE                                                                 
070100       MOVE MFS-ROER-EJ-FAELT        TO MOD-TISTADAT                      
070200     END-IF                                                               
070300*                                                                         
070400     EJECT                                                                
070500     .                                                                    
070600 D-KOLLA-O-UPPDATERA SECTION.                                             
070700                                                                          
070800     MOVE WS-KDFRAKT                   TO W-KDFRAKT                       
070900     MOVE WS-KDORDKL                   TO W-KDORDKL                       
071000     PERFORM IMS-GHNP-WDB414                                              
071100     IF (MOD-FLAENDR-UT = 'J' OR 'Y') AND SEGMENT-FINNS                   
071200       PERFORM IMS-DELETE-WDB414                                          
071300       PERFORM MFS-RENSA-BILD                                             
071400       MOVE MFS-RENSA-FAELT            TO MOD-FLAENDR-UT                  
071500       MOVE TEXT-0415 (SPRAK-IX)       TO MOD-TEMFSFEL                    
071600*      ORDERINFORMATION BORTTAGEN                                         
071700     ELSE                                                                 
071800       PERFORM DA-KOLLA-INDATA                                            
071900       IF INDATA-OK                                                       
072000         IF TID-IFYLLD                                                    
072100           PERFORM DB-UPPDATERA                                           
072200         ELSE                                                             
072300           MOVE TEXT-0418 (SPRAK-IX)   TO MOD-TEMFSFEL                    
072400*          VECKODAG EJ IFYLLD                                             
072500         END-IF                                                           
072600       ELSE                                                               
072700         MOVE TEXT-0409 (SPRAK-IX)     TO MOD-TEMFSFEL                    
072800*        UPPLYSTA FÄLT FEL                                                
072900         PERFORM MFS-ROER-EJ-FALT                                         
073000       END-IF                                                             
073100     END-IF                                                               
073200                                                                          
073300     EJECT                                                                
073400     .                                                                    
073500 DA-KOLLA-INDATA SECTION.                                                 
073600                                                                          
073700     MOVE JA                           TO SW-INDATA-OK                    
073800     MOVE NEJ                          TO SW-TID-IFYLLD                   
073900     MOVE MID-KDROPACK                 TO BIPACK-TYP                      
074000     MOVE MID-IDSKYLT                  TO SPRAK-TYP                       
074100                                                                          
074200     IF SEGMENT-FINNS                                                     
074300       PERFORM DAA-ANDRING-INDATA                                         
074400     ELSE                                                                 
074500       PERFORM DAB-NYUPPLAGG-INDATA                                       
074600     END-IF                                                               
074700                                                                          
074800     IF MID-IDFTG-IN NOT = ALL '+'                                        
074900       MOVE MID-IDFTG-IN             TO WS-IDFTG                          
075000       IF NOT IDFTG-GODKAEND                                              
075100         MOVE MFS-NUM-FAELT-FEL      TO MOD-IDFTG-IN-ATTR                 
075200         MOVE NEJ                    TO SW-INDATA-OK                      
075300         MOVE MID-IDFTG-UT           TO MOD-IDFTG-UT                      
075400       ELSE                                                               
075500         MOVE MID-IDFTG-IN           TO MOD-IDFTG-UT                      
075600         MOVE MFS-RENSA-FAELT        TO MOD-IDFTG-IN                      
075700       END-IF                                                             
075800     END-IF                                                               
075900                                                                          
076000     EJECT                                                                
076100     .                                                                    
076200 DAA-ANDRING-INDATA SECTION.                                              
076300                                                                          
076400     IF MID-IDFTG-IN = ALL '+'                                            
076500        MOVE ORD-IDFTG               TO ARB-IDFTG                         
076600     ELSE                                                                 
076700        MOVE MID-IDFTG-IN            TO ARB-IDFTG                         
076800     END-IF                                                               
076900                                                                          
077000     IF MID-BEKUNDRF-001 NOT = ALL '+'                                    
077100       MOVE MID-BEKUNDRF-001         TO MOD-BEKUNDRF-001                  
077200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-BEKUNDRF-ATTR                 
077300     ELSE                                                                 
077400       MOVE MFS-ROER-EJ-FAELT        TO MOD-BEKUNDRF-ATTR                 
077500     END-IF                                                               
077600                                                                          
077700     IF MID-IDSKYLT NOT = ALL '+'                                         
077800       IF GODK-SPRAK                                                      
077900         MOVE MID-IDSKYLT            TO MOD-IDSKYLT                       
078000         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDSKYLT-ATTR                  
078100       ELSE                                                               
078200         MOVE SPACE                  TO MOD-KXSPRAK                       
078300       END-IF                                                             
078400     ELSE                                                                 
078500       MOVE MFS-ROER-EJ-FAELT        TO MOD-IDSKYLT-ATTR                  
078600                                        MOD-KXSPRAK-ATTR                  
078700     END-IF                                                               
078800                                                                          
078900     IF MID-KDROPACK NOT = ALL '+'                                        
079000       IF GODK-BIPACKKOD                                                  
079100         MOVE MID-KDROPACK           TO MOD-KDROPACK                      
079200         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDROPACK-ATTR                 
079300       ELSE                                                               
079400         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDROPACK-ATTR                 
079500         MOVE NEJ                    TO SW-INDATA-OK                      
079600       END-IF                                                             
079700     ELSE                                                                 
079800       MOVE MFS-ROER-EJ-FAELT        TO MOD-KDROPACK-ATTR                 
079900                                        MOD-KXROPACK-ATTR                 
080000     END-IF                                                               
080100                                                                          
080200     IF MID-KDFAKTYP NOT = '+'                                            
080300       MOVE MID-KDFAKTYP             TO FAKTURA-TYP                       
080400       IF GODK-FAKTURATYP                                                 
080500         MOVE MID-KDFAKTYP           TO MOD-KDFAKTYP                      
080600         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDFAKTYP-ATTR                 
080700       ELSE                                                               
080800         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDFAKTYP-ATTR                 
080900         MOVE NEJ                    TO SW-INDATA-OK                      
081000       END-IF                                                             
081100     ELSE                                                                 
081200       MOVE ORD-KDFAKTYP             TO FAKTURA-TYP                       
081300       MOVE MFS-ROER-EJ-FAELT        TO MOD-KDFAKTYP-ATTR                 
081400                                        MOD-KXFAKTYP-ATTR                 
081500     END-IF                                                               
081600                                                                          
081700     IF GODK-FAKTYP-KONTO                                                 
081800       PERFORM DAAA-BEHANDLA-GN-FAKTURA                                   
081900     ELSE                                                                 
082000       PERFORM DAAB-BEHANDLA-RK-FAKTURA                                   
082100     END-IF                                                               
082200                                                                          
082300     IF MID-KDNOTES NOT = ALL '+'                                         
082400       MOVE MID-KDNOTES              TO MOD-KDNOTES                       
082500       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-KDNOTES-ATTR                  
082600     ELSE                                                                 
082700       MOVE MFS-ROER-EJ-FAELT        TO MOD-KDNOTES-ATTR                  
082800     END-IF                                                               
082900                                                                          
083000     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10                         
083100       IF MID-TID (IX) NOT = '+'                                          
083200         IF MID-TID (IX) = '0'                                            
083300           MOVE MFS-ALFA-FAELT-FEL   TO MOD-TID-ATTR (IX)                 
083400           MOVE NEJ                  TO SW-INDATA-OK                      
083500         ELSE                                                             
083600           IF MID-TID (IX) NOT = SPACE                                    
083700             MOVE JA                 TO SW-TID-IFYLLD                     
083800             MOVE 'X'                TO MOD-TID (IX)                      
083900             MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TID-ATTR (IX)              
084000           ELSE                                                           
084100             MOVE SPACE              TO MOD-TID (IX)                      
084200             MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TID-ATTR (IX)              
084300           END-IF                                                         
084400         END-IF                                                           
084500       ELSE                                                               
084600         IF (ORD-TID (IX) NOT = ZERO) AND                                 
084700            (MID-TID (IX) NOT = SPACE)                                    
084800           MOVE MFS-ROER-EJ-FAELT    TO MOD-TID-ATTR (IX)                 
084900           MOVE JA                   TO SW-TID-IFYLLD                     
085000         END-IF                                                           
085100       END-IF                                                             
085200     END-PERFORM                                                          
085300                                                                          
085400     IF MID-TISTADAT NOT = ALL '+'                                        
085500       ACCEPT ARB-DATUM FROM DATE                                         
085600       MOVE MID-TISTADAT   TO TMP1-YYMMDD                                 
085700       MOVE ARB-DATUM      TO TMP2-YYMMDD                                 
085800       PERFORM WY2000P1                                                   
085900       IF  MID-TISTADAT NUMERIC  AND                                      
086000           TMP1-YYMMDD  >= TMP2-YYMMDD                                    
086100         MOVE MID-TISTADAT           TO MOD-TISTADAT                      
086200         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TISTADAT-ATTR                 
086300       ELSE                                                               
086400         MOVE MFS-NUM-FAELT-FEL      TO MOD-TISTADAT-ATTR                 
086500         MOVE NEJ                    TO SW-INDATA-OK                      
086600       END-IF                                                             
086700     ELSE                                                                 
086800       IF ORD-TISTADAT > ZERO AND ORD-TISTADAT NUMERIC                    
086900         MOVE MFS-ROER-EJ-FAELT      TO MOD-TISTADAT-ATTR                 
087000       ELSE                                                               
087100         MOVE MFS-NUM-FAELT-FEL      TO MOD-TISTADAT-ATTR                 
087200         MOVE NEJ                    TO SW-INDATA-OK                      
087300       END-IF                                                             
087400     END-IF                                                               
087500                                                                          
087600     EJECT                                                                
087700     .                                                                    
087800 DAAA-BEHANDLA-GN-FAKTURA SECTION.                                        
087900                                                                          
088000     IF MID-IDKONTO-IN NOT = ALL '+'                                      
088100****** IF MID-KDROPACK NOT = ALL '+'                                      
088200* SG      MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDROPACK-ATTR                   
088300*         MOVE NEJ                 TO SW-INDATA-OK                        
088400****** END-IF                                                             
088500       MOVE MID-IDKONTO-IN         TO ARB-IDKONTO-2                       
088600**SG** MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKONTO-IN                      
088700     ELSE                                                                 
088800       MOVE ORD-IDKONTO            TO ARB-KONTO                           
088900     END-IF                                                               
089000     INSPECT ARB-IDKONTO-2 REPLACING LEADING SPACE BY ZERO                
089100     INSPECT MID-IDKONTO-UT REPLACING LEADING SPACE BY ZERO               
089200     IF MID-IDKONTO-UT NUMERIC AND MID-IDKONTO-UT > ZERO                  
089300       IF MID-IDKONTO-IN NOT = ALL '+'                                    
089400         MOVE MID-IDKONTO-IN       TO ARB-IDKONTO-2                       
089500       ELSE                                                               
089600         MOVE MID-IDKONTO-UT       TO ARB-IDKONTO-2                       
089700       END-IF                                                             
089800     END-IF                                                               
089900                                                                          
090000     IF ARB-IDKONTO-2 NUMERIC AND ARB-IDKONTO-2 > ZERO                    
090100       MOVE ARB-IDKONTO-2          TO MOD-IDKONTO-UT                      
090200       MOVE MFS-RENSA-FAELT        TO MOD-IDKONTO-IN                      
090300       INSPECT MOD-IDKONTO-UT REPLACING LEADING ZERO BY SPACE             
090400     ELSE                                                                 
090500       MOVE MFS-NUM-FAELT-FEL      TO MOD-IDKONTO-IN-ATTR                 
090600       MOVE NEJ                    TO SW-INDATA-OK                        
090700     END-IF                                                               
090800                                                                          
090900     IF MID-IDKST-IN = ALL '+'                                            
091000       MOVE ORD-IDKST              TO ARB-IDKST                           
091100     ELSE                                                                 
091200       MOVE MID-IDKST-IN           TO ARB-IDKST                           
091300       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKST-IN                        
091400     END-IF                                                               
091500                                                                          
091600                                                                          
091700     IF MID-IDKST-UT > SPACE                                              
091800      OR (DIST20-EMBALLAGE                                                
091900      AND MID-IDKST-UT = SPACE)                                           
092000      IF MID-IDKST-IN NOT = ALL '+'                                       
092100        MOVE MID-IDKST-IN          TO ARB-IDKST                           
092200      ELSE                                                                
092300        MOVE MID-IDKST-UT          TO ARB-IDKST                           
092400      END-IF                                                              
092500     END-IF                                                               
092600     IF ARB-IDKST > SPACE                                                 
092700      OR (DIST20-EMBALLAGE                                                
092800      AND ARB-IDKST = SPACE)                                              
092900      MOVE ARB-IDKST               TO MOD-IDKST-UT                        
093000      MOVE MFS-RENSA-FAELT         TO MOD-IDKST-IN                        
093100     ELSE                                                                 
093200      MOVE MFS-NUM-FAELT-FEL       TO MOD-IDKST-IN-ATTR                   
093300      MOVE NEJ                     TO SW-INDATA-OK                        
093400     END-IF                                                               
093500                                                                          
093600     PERFORM S02-IDANALYS-KONTROLL                                        
093700                                                                          
093800     IF MID-BEVARREF NOT = ALL SPACE                                      
093900       IF MID-BEVARREF NOT = ALL '+'                                      
094000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEVARREF-ATTR                   
094100         MOVE NEJ                  TO SW-INDATA-OK                        
094200       ELSE                                                               
094300         MOVE SPACE                TO MOD-BEVARREF                        
094400       END-IF                                                             
094500     END-IF                                                               
094600     EJECT                                                                
094700     .                                                                    
094800                                                                          
094900 DAAB-BEHANDLA-RK-FAKTURA SECTION.                                        
095000                                                                          
095100     IF MID-IDKONTO-IN NOT = ALL '+'                                      
095200       IF NOT SPEC-BIPACKKOD                                              
095300          MOVE MFS-NUM-FAELT-FEL  TO MOD-IDKONTO-IN-ATTR                  
095400          MOVE NEJ                TO SW-INDATA-OK                         
095500       ELSE                                                               
095600          MOVE MID-IDKONTO-IN         TO ARB-IDKONTO-2                    
095700          MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKONTO-IN                   
095800       END-IF                                                             
095900     ELSE                                                                 
096000       MOVE ORD-IDKONTO            TO ARB-KONTO                           
096100     END-IF                                                               
096200     INSPECT ARB-IDKONTO-2 REPLACING LEADING SPACE BY ZERO                
096300     INSPECT MID-IDKONTO-UT REPLACING LEADING SPACE BY ZERO               
096400     IF MID-IDKONTO-UT NUMERIC AND MID-IDKONTO-UT > ZERO                  
096500       IF MID-IDKONTO-IN NOT = ALL '+'                                    
096600         MOVE MID-IDKONTO-IN       TO ARB-IDKONTO-2                       
096700       ELSE                                                               
096800         MOVE MID-IDKONTO-UT       TO ARB-IDKONTO-2                       
096900       END-IF                                                             
097000     END-IF                                                               
097100     IF ARB-IDKONTO-2 NUMERIC AND ARB-IDKONTO-2 > ZERO                    
097200       MOVE ARB-IDKONTO-2          TO MOD-IDKONTO-UT                      
097300       MOVE MFS-RENSA-FAELT        TO MOD-IDKONTO-IN                      
097400       INSPECT MOD-IDKONTO-UT REPLACING LEADING ZERO BY SPACE             
097500     END-IF                                                               
097600                                                                          
097700                                                                          
097800     IF MID-IDKST-IN = ALL '+'                                            
097900       MOVE ORD-IDKST              TO ARB-IDKST                           
098000     ELSE                                                                 
098100       MOVE MID-IDKST-IN           TO ARB-IDKST                           
098200       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKST-IN                        
098300     END-IF                                                               
098400     IF MID-IDKST-UT > SPACE                                              
098500       IF MID-IDKST-IN = ALL '+'                                          
098600         MOVE MID-IDKST-UT         TO ARB-IDKST                           
098700       ELSE                                                               
098800         MOVE MID-IDKST-IN         TO ARB-IDKST                           
098900       END-IF                                                             
099000     END-IF                                                               
099100     IF  ARB-IDKST > SPACE                                                
099200       MOVE ARB-IDKST              TO MOD-IDKST-UT                        
099300       MOVE MFS-RENSA-FAELT        TO MOD-IDKST-IN                        
099400     END-IF                                                               
099500                                                                          
099600     PERFORM S02-IDANALYS-KONTROLL                                        
099700                                                                          
099800     IF MID-BEVARREF NOT = ALL '+'                                        
099900        MOVE MID-BEVARREF          TO MOD-BEVARREF                        
100000     ELSE                                                                 
100100       IF ORD-BEVARREF NOT = SPACE                                        
100200         MOVE MFS-ROER-EJ-FAELT    TO MOD-BEVARREF-ATTR                   
100300       END-IF                                                             
100400       IF MID-KDROPACK NOT = ALL '+'                                      
100500          MOVE MFS-ALFA-FAELT-FEL  TO MOD-BEVARREF-ATTR                   
100600          MOVE NEJ                 TO SW-INDATA-OK                        
100700       END-IF                                                             
100800     END-IF                                                               
100900     EJECT                                                                
101000     .                                                                    
101100 DAB-NYUPPLAGG-INDATA SECTION.                                            
101200                                                                          
101300     IF MID-IDFTG-IN = ALL '+'                                            
101400        MOVE MFS-NUM-FAELT-FEL       TO MOD-IDFTG-IN-ATTR                 
101500        MOVE NEJ                     TO SW-INDATA-OK                      
101600     ELSE                                                                 
101700        MOVE MID-IDFTG-IN            TO ARB-IDFTG                         
101800     END-IF                                                               
101900                                                                          
102000     IF MID-BEKUNDRF-001 NOT = ALL '+'                                    
102100       MOVE MID-BEKUNDRF-001         TO MOD-BEKUNDRF-001                  
102200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-BEKUNDRF-ATTR                 
102300     END-IF                                                               
102400                                                                          
102500     IF MID-IDSKYLT NOT = ALL '+'                                         
102600       IF GODK-SPRAK                                                      
102700         MOVE MID-IDSKYLT            TO MOD-IDSKYLT                       
102800         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDSKYLT-ATTR                  
102900       ELSE                                                               
103000         MOVE SPACE                  TO MOD-KXSPRAK                       
103100       END-IF                                                             
103200     END-IF                                                               
103300                                                                          
103400     IF MID-KDROPACK NOT = ALL '+'                                        
103500       IF GODK-BIPACKKOD                                                  
103600         MOVE MID-KDROPACK           TO MOD-KDROPACK                      
103700         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDROPACK-ATTR                 
103800       ELSE                                                               
103900         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDROPACK-ATTR                 
104000         MOVE NEJ                    TO SW-INDATA-OK                      
104100       END-IF                                                             
104200     END-IF                                                               
104300                                                                          
104400     MOVE MID-KDFAKTYP               TO FAKTURA-TYP                       
104500     IF (MID-KDFAKTYP NOT = '+') AND (GODK-FAKTURATYP)                    
104600       MOVE MID-KDFAKTYP             TO MOD-KDFAKTYP                      
104700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-KDFAKTYP-ATTR                 
104800     ELSE                                                                 
104900       MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDFAKTYP-ATTR                 
105000       MOVE NEJ                      TO SW-INDATA-OK                      
105100     END-IF                                                               
105200                                                                          
105300     IF GODK-FAKTYP-KONTO                                                 
105400       PERFORM DABA-BEHANDLA-GN-FAKTURA                                   
105500     ELSE                                                                 
105600       PERFORM DABB-BEHANDLA-RK-FAKTURA                                   
105700     END-IF                                                               
105800                                                                          
105900                                                                          
106000     IF MID-KDNOTES NOT = ALL '+'                                         
106100       MOVE MID-KDNOTES              TO MOD-KDNOTES                       
106200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-KDNOTES-ATTR                  
106300     END-IF                                                               
106400                                                                          
106500     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10                         
106600       IF MID-TID (IX) NOT = '+'                                          
106700         IF MID-TID (IX) = '0'                                            
106800           MOVE MFS-ALFA-FAELT-FEL   TO MOD-TID-ATTR (IX)                 
106900           MOVE NEJ                  TO SW-INDATA-OK                      
107000         ELSE                                                             
107100           IF MID-TID (IX) NOT = SPACE                                    
107200             MOVE JA                 TO SW-TID-IFYLLD                     
107300             MOVE 'X'                TO MOD-TID (IX)                      
107400             MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TID-ATTR (IX)              
107500           END-IF                                                         
107600         END-IF                                                           
107700       END-IF                                                             
107800     END-PERFORM                                                          
107900                                                                          
108000     IF MID-TISTADAT NOT = ALL '+'                                        
108100       ACCEPT ARB-DATUM FROM DATE                                         
108200       MOVE MID-TISTADAT   TO TMP1-YYMMDD                                 
108300       MOVE ARB-DATUM      TO TMP2-YYMMDD                                 
108400       PERFORM WY2000P1                                                   
108500       IF MID-TISTADAT NUMERIC AND                                        
108600          TMP1-YYMMDD  >= TMP2-YYMMDD                                     
108700         MOVE MID-TISTADAT           TO MOD-TISTADAT                      
108800         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TISTADAT-ATTR                 
108900       ELSE                                                               
109000         MOVE MFS-NUM-FAELT-FEL      TO MOD-TISTADAT-ATTR                 
109100         MOVE NEJ                    TO SW-INDATA-OK                      
109200       END-IF                                                             
109300     ELSE                                                                 
109400       MOVE MFS-NUM-FAELT-FEL        TO MOD-TISTADAT-ATTR                 
109500       MOVE NEJ                      TO SW-INDATA-OK                      
109600     END-IF                                                               
109700                                                                          
109800     .                                                                    
109900     EJECT                                                                
110000 DABA-BEHANDLA-GN-FAKTURA SECTION.                                        
110100                                                                          
110200     IF MID-IDKONTO-IN NOT = ALL '+'                                      
110300****** IF MID-KDROPACK NOT = ALL '+'                                      
110400* SG      MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDROPACK-ATTR                   
110500*         MOVE NEJ                 TO SW-INDATA-OK                        
110600****** END-IF                                                             
110700       MOVE MID-IDKONTO-IN         TO ARB-IDKONTO-2                       
110800***SG* MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKONTO-IN                      
110900     ELSE                                                                 
111000       MOVE MID-IDKONTO-UT         TO ARB-IDKONTO-2                       
111100     END-IF                                                               
111200     INSPECT ARB-IDKONTO-2 REPLACING LEADING SPACE BY ZERO                
111300     INSPECT MID-IDKONTO-UT REPLACING LEADING SPACE BY ZERO               
111400     IF MID-IDKONTO-UT NUMERIC AND MID-IDKONTO-UT > ZERO                  
111500       IF MID-IDKONTO-IN NOT = ALL '+'                                    
111600         MOVE MID-IDKONTO-IN       TO ARB-IDKONTO-2                       
111700       ELSE                                                               
111800         MOVE MID-IDKONTO-UT       TO ARB-IDKONTO-2                       
111900       END-IF                                                             
112000     END-IF                                                               
112100     IF ARB-IDKONTO-2 NUMERIC AND ARB-IDKONTO-2 > ZERO                    
112200       MOVE ARB-IDKONTO-2          TO MOD-IDKONTO-UT                      
112300       MOVE MFS-RENSA-FAELT        TO MOD-IDKONTO-IN                      
112400       INSPECT MOD-IDKONTO-UT REPLACING LEADING ZERO BY SPACE             
112500     ELSE                                                                 
112600       MOVE MFS-NUM-FAELT-FEL    TO MOD-IDKONTO-IN-ATTR                   
112700       MOVE NEJ                  TO SW-INDATA-OK                          
112800     END-IF                                                               
112900                                                                          
113000                                                                          
113100     IF MID-IDKST-IN NOT = ALL '+'                                        
113200       MOVE MID-IDKST-IN           TO ARB-IDKST                           
113300       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKST-IN                        
113400     ELSE                                                                 
113500       MOVE MID-IDKST-UT           TO ARB-IDKST                           
113600     END-IF                                                               
113700     IF MID-IDKST-UT > SPACE                                              
113800      OR (DIST20-EMBALLAGE                                                
113900      AND MID-IDKST-UT = SPACE)                                           
114000      IF MID-IDKST-IN NOT = ALL '+'                                       
114100        MOVE MID-IDKST-IN          TO ARB-IDKST                           
114200      ELSE                                                                
114300        MOVE MID-IDKST-UT          TO ARB-IDKST                           
114400      END-IF                                                              
114500     END-IF                                                               
114600     IF ARB-IDKST > SPACE                                                 
114700      OR (DIST20-EMBALLAGE                                                
114800      AND ARB-IDKST = SPACE)                                              
114900      MOVE ARB-IDKST               TO MOD-IDKST-UT                        
115000      MOVE MFS-RENSA-FAELT         TO MOD-IDKST-IN                        
115100     ELSE                                                                 
115200      MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKST-IN-ATTR                     
115300      MOVE NEJ                   TO SW-INDATA-OK                          
115400     END-IF                                                               
115500                                                                          
115600     PERFORM S02-IDANALYS-KONTROLL                                        
115700                                                                          
115800     IF MID-BEVARREF NOT = ALL SPACE                                      
115900       IF MID-BEVARREF NOT = ALL '+'                                      
116000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEVARREF-ATTR                   
116100         MOVE NEJ                  TO SW-INDATA-OK                        
116200       END-IF                                                             
116300     END-IF                                                               
116400     EJECT                                                                
116500     .                                                                    
116600 DABB-BEHANDLA-RK-FAKTURA SECTION.                                        
116700                                                                          
116800     IF MID-IDKONTO-IN NOT = ALL '+'                                      
116900       IF NOT SPEC-BIPACKKOD                                              
117000          MOVE MFS-NUM-FAELT-FEL      TO MOD-IDKONTO-IN-ATTR              
117100          MOVE NEJ                    TO SW-INDATA-OK                     
117200       ELSE                                                               
117300          MOVE MID-IDKONTO-IN         TO ARB-IDKONTO-2                    
117400          MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKONTO-IN                   
117500       END-IF                                                             
117600     END-IF                                                               
117700     INSPECT ARB-IDKONTO-2 REPLACING LEADING SPACE BY ZERO                
117800     INSPECT MID-IDKONTO-UT REPLACING LEADING SPACE BY ZERO               
117900     IF MID-IDKONTO-UT NUMERIC AND MID-IDKONTO-UT > ZERO                  
118000       IF MID-IDKONTO-IN NOT = ALL '+'                                    
118100         MOVE MID-IDKONTO-IN       TO ARB-IDKONTO-2                       
118200       ELSE                                                               
118300         MOVE MID-IDKONTO-UT       TO ARB-IDKONTO-2                       
118400       END-IF                                                             
118500     END-IF                                                               
118600     IF ARB-IDKONTO-2 NUMERIC AND ARB-IDKONTO-2 > ZERO                    
118700       MOVE ARB-IDKONTO-2          TO MOD-IDKONTO-UT                      
118800       MOVE MFS-RENSA-FAELT        TO MOD-IDKONTO-IN                      
118900       INSPECT MOD-IDKONTO-UT REPLACING LEADING ZERO BY SPACE             
119000     END-IF                                                               
119100                                                                          
119200                                                                          
119300     IF MID-IDKST-IN NOT = ALL '+'                                        
119400       MOVE MID-IDKST-IN           TO ARB-IDKST                           
119500       MOVE MFS-ROER-EJ-FAELT      TO MOD-IDKST-IN                        
119600     END-IF                                                               
119700     IF MID-IDKST-UT > SPACE                                              
119800       IF MID-IDKST-IN = ALL '+'                                          
119900         MOVE MID-IDKST-UT         TO ARB-IDKST                           
120000       ELSE                                                               
120100         MOVE MID-IDKST-IN         TO ARB-IDKST                           
120200         MOVE MFS-RENSA-FAELT      TO MOD-IDKST-UT                        
120300       END-IF                                                             
120400     END-IF                                                               
120500     IF ARB-IDKST > SPACE                                                 
120600       MOVE ARB-IDKST              TO MOD-IDKST-UT                        
120700       MOVE MFS-RENSA-FAELT        TO MOD-IDKST-IN                        
120800     END-IF                                                               
120900                                                                          
121000     PERFORM S02-IDANALYS-KONTROLL                                        
121100                                                                          
121200     IF MID-BEVARREF NOT = ALL '+'                                        
121300         MOVE MID-BEVARREF           TO MOD-BEVARREF                      
121400         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-BEVARREF-ATTR                 
121500     ELSE                                                                 
121600         IF SPEC-BIPACKKOD                                                
121700            MOVE MFS-ALFA-FAELT-FEL  TO MOD-BEVARREF-ATTR                 
121800            MOVE NEJ                 TO SW-INDATA-OK                      
121900         END-IF                                                           
122000     END-IF                                                               
122100     EJECT                                                                
122200     .                                                                    
122300 DB-UPPDATERA SECTION.                                                    
122400                                                                          
122500     PERFORM IMS-GHU-WDB414                                               
122600                                                                          
122700     IF MID-BEKUNDRF-001 NOT = ALL '+'                                    
122800       MOVE MID-BEKUNDRF-001           TO ORD-BEKUNDRF                    
122900     ELSE                                                                 
123000       IF SEGMENT-SAKNAS                                                  
123100         MOVE SPACE                    TO ORD-BEKUNDRF                    
123200       END-IF                                                             
123300     END-IF                                                               
123400                                                                          
123500     IF MID-IDSKYLT NOT = ALL '+'                                         
123600       IF GODK-SPRAK                                                      
123700         MOVE MID-IDSKYLT              TO ORD-IDSKYLT                     
123800       ELSE                                                               
123900         MOVE SPACE                    TO ORD-IDSKYLT                     
124000       END-IF                                                             
124100     ELSE                                                                 
124200       IF SEGMENT-SAKNAS                                                  
124300         MOVE SPACE                    TO ORD-IDSKYLT                     
124400       END-IF                                                             
124500     END-IF                                                               
124600                                                                          
124700     IF MID-BEVARREF NOT = ALL '+'                                        
124800       MOVE MID-BEVARREF               TO ORD-BEVARREF                    
124900     ELSE                                                                 
125000       IF SEGMENT-SAKNAS                                                  
125100         MOVE SPACE                    TO ORD-BEVARREF                    
125200       END-IF                                                             
125300     END-IF                                                               
125400                                                                          
125500     IF MID-KDROPACK NOT = ALL '+'                                        
125600       MOVE MID-KDROPACK               TO ORD-KDROPACK (1)                
125700                                          ORD-KDROPACK (2)                
125800     ELSE                                                                 
125900       IF SEGMENT-SAKNAS                                                  
126000         MOVE SPACE                    TO ORD-KDROPACK (1)                
126100                                          ORD-KDROPACK (2)                
126200       END-IF                                                             
126300     END-IF                                                               
126400                                                                          
126500     IF MID-IDFTG-IN NOT = ALL '+'                                        
126600       MOVE MID-IDFTG-IN               TO ORD-IDFTG                       
126700     ELSE                                                                 
126800       IF SEGMENT-SAKNAS                                                  
126900         MOVE ZERO                     TO ORD-IDFTG                       
127000       END-IF                                                             
127100     END-IF                                                               
127200                                                                          
127300     IF MID-IDKONTO-IN NOT = ALL '+'                                      
127400       MOVE MID-IDKONTO-IN             TO ORD-IDKONTO                     
127500     ELSE                                                                 
127600       IF ARB-IDKONTO-2 NUMERIC AND ARB-IDKONTO-2 > ZERO                  
127700         MOVE ARB-IDKONTO-2            TO ORD-IDKONTO                     
127800       ELSE                                                               
127900         IF SEGMENT-SAKNAS                                                
128000           MOVE ZERO                   TO ORD-IDKONTO                     
128100         END-IF                                                           
128200       END-IF                                                             
128300     END-IF                                                               
128400                                                                          
128500     IF MID-IDANALYS-IN = ALL '+'                                         
128600       IF MID-IDANALYS-IN NUMERIC AND ARB-IDANALYS > ZERO                 
128700         MOVE MID-IDANALYS-IN          TO ORD-IDANALYS                    
128800       ELSE                                                               
128900         IF SEGMENT-SAKNAS                                                
129000           MOVE ZERO                   TO ORD-IDANALYS                    
129100         END-IF                                                           
129200       END-IF                                                             
129300     ELSE                                                                 
129400       MOVE MID-IDANALYS-IN            TO ORD-IDANALYS                    
129500     END-IF                                                               
129600                                                                          
129700     IF MID-KDFAKTYP NOT = ALL '+'                                        
129800       MOVE MID-KDFAKTYP               TO ORD-KDFAKTYP                    
129900     ELSE                                                                 
130000       IF SEGMENT-SAKNAS                                                  
130100         MOVE SPACE                    TO ORD-KDFAKTYP                    
130200       END-IF                                                             
130300     END-IF                                                               
130400                                                                          
130500     IF MID-IDKST-IN NOT = ALL '+'                                        
130600       MOVE MID-IDKST-IN               TO ORD-IDKST                       
130700     ELSE                                                                 
130800       IF ARB-IDKST > SPACE                                               
130900         MOVE ARB-IDKST                TO ORD-IDKST                       
131000       ELSE                                                               
131100         IF SEGMENT-SAKNAS                                                
131200           MOVE SPACE                    TO ORD-IDKST                     
131300         END-IF                                                           
131400       END-IF                                                             
131500     END-IF                                                               
131600                                                                          
131700     IF MID-KDNOTES NOT = ALL '+'                                         
131800       MOVE MID-KDNOTES                TO ORD-KDNOTES                     
131900     ELSE                                                                 
132000       IF SEGMENT-SAKNAS                                                  
132100         MOVE SPACE                    TO ORD-KDNOTES                     
132200       END-IF                                                             
132300     END-IF                                                               
132400                                                                          
132500     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10                         
132600       IF MID-TID (IX) NOT = '+'                                          
132700         IF MID-TID (IX) = SPACE                                          
132800           MOVE ZERO                   TO ORD-TID (IX)                    
132900           MOVE MFS-RENSA-FAELT        TO MOD-TID (IX)                    
133000         ELSE                                                             
133100           MOVE MID-TID (IX)           TO ORD-TID (IX)                    
133200         END-IF                                                           
133300       ELSE                                                               
133400         IF SEGMENT-SAKNAS                                                
133500           MOVE ZERO                   TO ORD-TID (IX)                    
133600         END-IF                                                           
133700       END-IF                                                             
133800     END-PERFORM                                                          
133900                                                                          
134000     IF MID-TISTADAT NOT = ALL '+'                                        
134100       IF MID-TISTADAT = ALL SPACE                                        
134200         MOVE ZERO                     TO ORD-TISTADAT                    
134300       ELSE                                                               
134400         MOVE MID-TISTADAT             TO ORD-TISTADAT                    
134500       END-IF                                                             
134600     ELSE                                                                 
134700       IF SEGMENT-SAKNAS                                                  
134800         MOVE ZERO                     TO ORD-TISTADAT                    
134900       END-IF                                                             
135000     END-IF                                                               
135100                                                                          
135200     MOVE WS-IDDISTR                   TO W-IDDISTR                       
135300     MOVE WS-IDKUNDNR                  TO W-IDKUNDNR                      
135400     MOVE WS-KDFRAKT                   TO W-KDFRAKT                       
135500                                          ORD-KDFRAKT                     
135600     MOVE WS-KDORDKL                   TO W-KDORDKL                       
135700                                          ORD-KDORDKL                     
135800     IF SEGMENT-FINNS                                                     
135900       PERFORM IMS-REPL-WDB414                                            
136000     ELSE                                                                 
136100       PERFORM IMS-INSERT-WDB414                                          
136200     END-IF                                                               
136300     PERFORM S011-LAGG-UT-INFOTEXTER                                      
136400     MOVE TEXT-0404 (SPRAK-IX)         TO MOD-TEMFSFEL                    
136500*    UPPDATERING UTFÖRD                                                   
136600     .                                                                    
136700     EJECT                                                                
136800 E-LAS-WDB401 SECTION.                                                    
136900                                                                          
137000     MOVE WS-IDDISTR                   TO W-IDDISTR                       
137100     MOVE WS-IDKUNDNR                  TO W-IDKUNDNR                      
137200     PERFORM IMS-GU-WDB401                                                
137300     IF SEGMENT-SAKNAS                                                    
137400       IF NOT DATA-IFYLLD                                                 
137500         PERFORM MFS-RENSA-BILD                                           
137600         MOVE TEXT-0413 (SPRAK-IX)     TO MOD-TEMFSFEL                    
137700*        INFORMATION SAKNAS                                               
137800*        MOVE TEXT-0417 (SPRAK-IX)     TO MOD-TEMFSFEL                    
137900**       ORDERHUVUD SAKNAS                                                
138000       END-IF                                                             
138100     ELSE                                                                 
138200       MOVE WS-KDFRAKT                 TO W-KDFRAKT                       
138300       MOVE WS-KDORDKL                 TO W-KDORDKL                       
138400       PERFORM IMS-GNP-WDB414                                             
138500       IF SEGMENT-SAKNAS                                                  
138600         IF NOT DATA-IFYLLD                                               
138700           PERFORM MFS-RENSA-BILD                                         
138800           MOVE TEXT-0413 (SPRAK-IX)   TO MOD-TEMFSFEL                    
138900*          INFORMATION SAKNAS                                             
139000         END-IF                                                           
139100       ELSE                                                               
139200         PERFORM S01-FLYTTA-ORDERINFO                                     
139300       END-IF                                                             
139400     END-IF                                                               
139500                                                                          
139600     .                                                                    
139700     EJECT                                                                
139800 S01-FLYTTA-ORDERINFO SECTION.                                            
139900                                                                          
140000     MOVE ORD-TISTADAT                 TO ARB-TISTADAT                    
140100     MOVE ORD-BEKUNDRF                 TO MOD-BEKUNDRF-001                
140200     MOVE ORD-IDSKYLT                  TO MOD-IDSKYLT                     
140300     MOVE ORD-BEVARREF                 TO MOD-BEVARREF                    
140400     MOVE ORD-KDROPACK (2)             TO MOD-KDROPACK                    
140500     MOVE ORD-IDKONTO                  TO ARB-KONTO                       
140600     MOVE ARB-IDKONTO-2                TO MOD-IDKONTO-UT                  
140700     INSPECT MOD-IDKONTO-UT REPLACING LEADING ZERO BY SPACE               
140800     MOVE ORD-KDFAKTYP                 TO MOD-KDFAKTYP                    
140900     MOVE ORD-IDFTG                    TO MOD-IDFTG-UT                    
141000     INSPECT MOD-IDFTG-UT REPLACING LEADING ZERO BY SPACE                 
141100     MOVE ORD-IDANALYS                 TO MOD-IDANALYS-UT                 
141200     INSPECT MOD-IDANALYS-UT REPLACING LEADING ZERO BY SPACE              
141300     MOVE ORD-IDKST                    TO MOD-IDKST-UT                    
141400     MOVE ORD-KDNOTES                  TO MOD-KDNOTES                     
141500     MOVE ARB-TISTADAT                 TO MOD-TISTADAT                    
141600                                                                          
141700     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10                         
141800       IF ORD-TID (IX) = ZERO                                             
141900         MOVE SPACE                    TO MOD-TID (IX)                    
142000       ELSE                                                               
142100         MOVE 'X'                      TO MOD-TID (IX)                    
142200       END-IF                                                             
142300     END-PERFORM                                                          
142400                                                                          
142500     PERFORM S011-LAGG-UT-INFOTEXTER                                      
142600     .                                                                    
142700     EJECT                                                                
142800 S02-IDANALYS-KONTROLL   SECTION.                                         
142900                                                                          
143000*SAP TILLÄGG                                                              
143100     IF FAKTURA-TYP = 'G' OR 'N'                                          
143200       IF MID-IDANALYS-IN = ALL '+'                                       
143300         IF MID-IDANALYS-UT = ALL '+'                                     
143400           MOVE ORD-IDANALYS         TO ARB-IDANALYS                      
143500*TAG FR. WDB4                                                             
143600         ELSE                                                             
143700           MOVE MID-IDANALYS-UT      TO ARB-IDANALYS                      
143800         END-IF                                                           
143900       ELSE                                                               
144000         MOVE MID-IDANALYS-IN        TO ARB-IDANALYS                      
144100         MOVE MFS-ROER-EJ-FAELT      TO MOD-IDANALYS-IN                   
144200       END-IF                                                             
144300       INSPECT ARB-IDANALYS    REPLACING LEADING SPACE BY ZERO            
144400       IF ARB-IDANALYS NUMERIC AND ARB-IDANALYS > ZERO                    
144500         MOVE MFS-NUM-FAELT-RAETT    TO MOD-IDANALYS-IN-ATTR              
144600         MOVE ARB-IDANALYS           TO MOD-IDANALYS-UT                   
144700         MOVE MFS-RENSA-FAELT        TO MOD-IDANALYS-IN                   
144800       ELSE                                                               
144900         MOVE MFS-NUM-FAELT-FEL      TO MOD-IDANALYS-IN-ATTR              
145000         MOVE NEJ                    TO SW-INDATA-OK                      
145100       END-IF                                                             
145200                                                                          
145300       IF INDATA-OK                                                       
145400*SAP   FLYTTA TILL W411SAP-AREA HÄR + EV.TAG BORT FR.W411OHFKAREA.        
145500         MOVE MSGI-IDFTG TO W-IDFTG-B6                                    
145600*        IF IDFTG-PV                                                      
145700*           MOVE 'SEPV'          TO SAP-KDTRADP                           
145800*        ELSE                                                             
145900*           IF IDFTG-CN                                                   
146000*              MOVE 'CN05'       TO SAP-KDTRADP                           
146100*           ELSE                                                          
146200*             IF IDFTG-IN                                                 
146300*                MOVE 'IN07'     TO SAP-KDTRADP                           
146400*             ELSE                                                        
146500*                MOVE 'SEPV'     TO SAP-KDTRADP                           
146600*           END-IF                                                        
146700*         END-IF                                                          
146800*        END-IF                                                           
146900         IF IDFTG-NON-VCC                                                 
146910           PERFORM IMS-GU-WDB601-FTG                                      
146920           IF SEGMENT-FINNS                                               
146930             MOVE DCS-KDTRADP    TO SAP-KDTRADP                           
146940           END-IF                                                         
146950         ELSE                                                             
146951           MOVE 'SEPV'           TO SAP-KDTRADP                           
146960         END-IF                                                           
147000                                                                          
147100         MOVE ARB-IDKST          TO SAP-IDKST                             
147200         MOVE ARB-KONTO          TO SAP-IDKONTO                           
147300         MOVE ARB-IDANALYS       TO SAP-IDANALYS                          
147400         MOVE SPACE              TO SAP-IDPROFIT                          
147500                                                                          
147600         IF MID-IDFTG-IN = ALL '+'                                        
147700            MOVE ZERO            TO SAP-IDDISTR                           
147800            MOVE SPACE           TO SAP-KDFAKTYP                          
147900            MOVE ZERO            TO SAP-IDFTG                             
148000            MOVE +2              TO SAP-KDCALL                            
148100         ELSE                                                             
148200            MOVE WS-IDDISTR      TO SAP-IDDISTR                           
148300            MOVE FAKTURA-TYP     TO SAP-KDFAKTYP                          
148400            MOVE ARB-IDFTG       TO SAP-IDFTG                             
148500            MOVE +1              TO SAP-KDCALL                            
148600         END-IF                                                           
148700                                                                          
148800         CALL W411SAP USING SAP-W411SAP WDH3-PCB                          
148900                                                                          
149000         IF SAP-IDFTG-OK = NEJ                                            
149100           MOVE NEJ               TO SW-INDATA-OK                         
149200         END-IF                                                           
149300                                                                          
149400         IF SAP-IDANALYS-OK = NEJ                                         
149500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDANALYS-IN-ATTR                 
149600           MOVE NEJ               TO SW-INDATA-OK                         
149700         END-IF                                                           
149800         IF SAP-IDKST-OK = NEJ                                            
149900           MOVE MFS-NUM-FAELT-FEL TO MOD-IDKST-IN-ATTR                    
150000           MOVE NEJ               TO SW-INDATA-OK                         
150100         END-IF                                                           
150200         IF SAP-IDKONTO-OK = NEJ                                          
150300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDKONTO-IN-ATTR                  
150400           MOVE NEJ               TO SW-INDATA-OK                         
150500         END-IF                                                           
150600         IF SAP-BEFEL NOT = SPACE                                         
150700           MOVE SAP-BEFEL         TO MOD-TEMFSINF                         
150800         END-IF                                                           
150900       END-IF                                                             
151000     ELSE                                                                 
151100       IF MID-IDANALYS-IN NOT = ALL '+'                                   
151200        INSPECT MID-IDANALYS-IN REPLACING LEADING SPACE BY ZERO           
151300        MOVE MID-IDANALYS-IN        TO MOD-IDANALYS-UT                    
151400        INSPECT MOD-IDANALYS-UT REPLACING LEADING ZERO BY SPACE           
151500       ELSE                                                               
151600        INSPECT MOD-IDANALYS-IN REPLACING LEADING ZERO BY SPACE           
151700        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDANALYS-UT                      
151800       END-IF                                                             
151900     END-IF                                                               
152000     .                                                                    
152100     SKIP2                                                                
152200 S011-LAGG-UT-INFOTEXTER SECTION.                                         
152300                                                                          
152400     MOVE MFS-RENSA-FAELT                      TO MOD-KXSPRAK             
152500                                                  MOD-KXROPACK            
152600                                                  MOD-KXFAKTYP            
152700     IF ORD-IDSKYLT = 'S  '                                               
152800       MOVE SVENSKA (SPRAK-IX)                 TO MOD-KXSPRAK             
152900     ELSE                                                                 
153000       IF ORD-IDSKYLT = 'GB '                                             
153100         MOVE ENGELSKA (SPRAK-IX)              TO MOD-KXSPRAK             
153200       ELSE                                                               
153300         IF ORD-IDSKYLT = 'D  '                                           
153400           MOVE TYSKA (SPRAK-IX)               TO MOD-KXSPRAK             
153500         ELSE                                                             
153600           IF ORD-IDSKYLT = 'F  '                                         
153700             MOVE FRANSKA (SPRAK-IX)           TO MOD-KXSPRAK             
153800           ELSE                                                           
153900             IF ORD-IDSKYLT = 'E  '                                       
154000               MOVE SPANSKA (SPRAK-IX)         TO MOD-KXSPRAK             
154100             ELSE                                                         
154200               MOVE MFS-RENSA-FAELT            TO MOD-KXSPRAK             
154300             END-IF                                                       
154400           END-IF                                                         
154500         END-IF                                                           
154600       END-IF                                                             
154700     END-IF                                                               
154800     IF ORD-KDROPACK (2) = '1'                                            
154900*    (ALLA-RO)                                                            
155000      MOVE KXROPACK-1 (SPRAK-IX)               TO MOD-KXROPACK            
155100     ELSE                                                                 
155200      IF ORD-KDROPACK (2) = '2'                                           
155300*     (SAMMA-ORDERKL)                                                     
155400        MOVE KXROPACK-2 (SPRAK-IX)             TO MOD-KXROPACK            
155500      ELSE                                                                
155600       IF ORD-KDROPACK (2) = '3'                                          
155700*      (SAMMA-LAGER+KLASS)                                                
155800         MOVE KXROPACK-3 (SPRAK-IX)            TO MOD-KXROPACK            
155900       ELSE                                                               
156000        IF ORD-KDROPACK (2) = '4'                                         
156100*       (SAMMA-FRAKT)                                                     
156200          MOVE KXROPACK-4 (SPRAK-IX)           TO MOD-KXROPACK            
156300        ELSE                                                              
156400         IF ORD-KDROPACK (2) = '5'                                        
156500*        (UTPEKANDE ORDER)                                                
156600           MOVE KXROPACK-5 (SPRAK-IX)          TO MOD-KXROPACK            
156700         ELSE                                                             
156800          IF ORD-KDROPACK (2) = '6'                                       
156900*         (UDDA/JÄMNT OCH FRAKT)                                          
157000            MOVE KXROPACK-6 (SPRAK-IX)         TO MOD-KXROPACK            
157100          ELSE                                                            
157200           IF ORD-KDROPACK (2) = '7'                                      
157300*          (BYTES)                                                        
157400             MOVE KXROPACK-7 (SPRAK-IX)        TO MOD-KXROPACK            
157500           ELSE                                                           
157600            IF ORD-KDROPACK (2) = '8'                                     
157700*           (EJ UTPEK ORD)                                                
157800              MOVE KXROPACK-8 (SPRAK-IX)       TO MOD-KXROPACK            
157900            ELSE                                                          
158000             IF ORD-KDROPACK (2) = 'A'                                    
158100*            (KLASS 3&4)                                                  
158200               MOVE KXROPACK-A (SPRAK-IX)      TO MOD-KXROPACK            
158300             ELSE                                                         
158400              IF ORD-KDROPACK (2) = 'B'                                   
158500*             (KLASS 2&3&4)                                               
158600                MOVE KXROPACK-B (SPRAK-IX)     TO MOD-KXROPACK            
158700              ELSE                                                        
158800               IF ORD-KDROPACK (2) = 'C'                                  
158900*              (KLASS 1)                                                  
159000                 MOVE KXROPACK-C (SPRAK-IX)    TO MOD-KXROPACK            
159100               ELSE                                                       
159200                IF ORD-KDROPACK (2) = 'D'                                 
159300*               (KLASS 1 & 2)                                             
159400                  MOVE KXROPACK-D (SPRAK-IX)   TO MOD-KXROPACK            
159500                ELSE                                                      
159600                 IF ORD-KDROPACK (2) = 'E'                                
159700*                (ALLA TPO)                                               
159800                   MOVE KXROPACK-E (SPRAK-IX)  TO MOD-KXROPACK            
159900                 ELSE                                                     
160000                  IF ORD-KDROPACK (2) = 'F'                               
160100*                 (SAMMA KONTO/KOSTN)                                     
160200                    MOVE KXROPACK-F (SPRAK-IX) TO MOD-KXROPACK            
160300                  ELSE                                                    
160400                   IF ORD-KDROPACK (2) = 'G'                              
160500*                  (SAMMA KLASS/FRAKT)                                    
160600                     MOVE KXROPACK-G (SPRAK-IX) TO MOD-KXROPACK           
160700                   ELSE                                                   
160800                   IF ORD-KDROPACK (2) = 'H'                              
160900*                  (ENDAST RO)                                            
161000                     MOVE KXROPACK-H (SPRAK-IX) TO MOD-KXROPACK           
161100                   ELSE                                                   
161200                   IF ORD-KDROPACK (2) = 'I'                              
161300*                  (ENDAST RO/SAMMA FK)                                   
161400                     MOVE KXROPACK-I (SPRAK-IX) TO MOD-KXROPACK           
161500                   ELSE                                                   
161600                   IF ORD-KDROPACK (2) = 'J'                              
161700*                  (RO KL 3 OCH 4)                                        
161800                     MOVE KXROPACK-J (SPRAK-IX) TO MOD-KXROPACK           
161900                   ELSE                                                   
162000                   IF ORD-KDROPACK (2) = 'K'                              
162100*                  (RO KL 2,3 OCH 4)                                      
162200                     MOVE KXROPACK-K (SPRAK-IX) TO MOD-KXROPACK           
162300                   ELSE                                                   
162400                   IF ORD-KDROPACK (2) = 'M'                              
162500*                  (TPO-3)                                                
162600                     MOVE KXROPACK-M (SPRAK-IX) TO MOD-KXROPACK           
162700                   ELSE                                                   
162800                     MOVE MFS-RENSA-FAELT      TO MOD-KXROPACK            
162900                   END-IF                                                 
163000                   END-IF                                                 
163100                   END-IF                                                 
163200                   END-IF                                                 
163300                   END-IF                                                 
163400                   END-IF                                                 
163500                  END-IF                                                  
163600                 END-IF                                                   
163700                END-IF                                                    
163800               END-IF                                                     
163900              END-IF                                                      
164000             END-IF                                                       
164100            END-IF                                                        
164200           END-IF                                                         
164300          END-IF                                                          
164400         END-IF                                                           
164500        END-IF                                                            
164600       END-IF                                                             
164700      END-IF                                                              
164800     END-IF                                                               
164900     IF ORD-KDFAKTYP = 'R'                                                
165000       MOVE EXTERN (SPRAK-IX)                  TO MOD-KXFAKTYP            
165100     ELSE                                                                 
165200       IF ORD-KDFAKTYP = 'G'                                              
165300         MOVE GRATIS (SPRAK-IX)                TO MOD-KXFAKTYP            
165400       ELSE                                                               
165500         IF ORD-KDFAKTYP = 'K'                                            
165600           MOVE KONSIGNATION (SPRAK-IX)        TO MOD-KXFAKTYP            
165700         ELSE                                                             
165800           IF ORD-KDFAKTYP = 'N'                                          
165900             MOVE INTERN (SPRAK-IX)            TO MOD-KXFAKTYP            
166000           ELSE                                                           
166100             MOVE MFS-RENSA-FAELT              TO MOD-KXFAKTYP            
166200           END-IF                                                         
166300         END-IF                                                           
166400       END-IF                                                             
166500     END-IF                                                               
166600                                                                          
166700     .                                                                    
166800     EJECT                                                                
166900                                                                          
167000 MFS-RENSA-NYCKLAR SECTION.                                               
167100                                                                          
167200     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                               
167300                             MOD-IDKUNDNR-UT                              
167400                             MOD-KDFRAKT-UT                               
167500                             MOD-KDORDKL-UT                               
167600                             MOD-FLAENDR-UT                               
167700                                                                          
167800                                                                          
167900     .                                                                    
168000     EJECT                                                                
168100 MFS-RENSA-BILD SECTION.                                                  
168200                                                                          
168300     MOVE MFS-RENSA-FAELT           TO MOD-IDDISTR-IN                     
168400                                       MOD-IDKUNDNR-IN                    
168500                                       MOD-KDFRAKT-IN                     
168600                                       MOD-KDORDKL-IN                     
168700                                       MOD-FLAENDR-IN                     
168800                                       MOD-TEMFSFEL                       
168900                                       MOD-TEMFSINF                       
169000                                       MOD-BEKUNDRF-001                   
169100                                       MOD-IDSKYLT                        
169200                                       MOD-KXSPRAK                        
169300                                       MOD-BEVARREF                       
169400                                       MOD-KDROPACK                       
169500                                       MOD-KXROPACK                       
169600                                       MOD-IDKONTO-IN                     
169700                                       MOD-IDKONTO-UT                     
169800                                       MOD-KDFAKTYP                       
169900                                       MOD-KXFAKTYP                       
170000                                       MOD-IDKST-IN                       
170100                                       MOD-IDKST-UT                       
170200                                       MOD-IDFTG-IN                       
170300                                       MOD-IDFTG-UT                       
170400                                       MOD-IDANALYS-IN                    
170500                                       MOD-IDANALYS-UT                    
170600                                       MOD-KDNOTES                        
170700                                       MOD-TISTADAT                       
170800                                                                          
170900     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10                         
171000       MOVE MFS-RENSA-FAELT         TO MOD-TID (IX)                       
171100     END-PERFORM                                                          
171200                                                                          
171300     .                                                                    
171400     EJECT                                                                
171500 MFS-ROER-EJ-FALT  SECTION.                                               
171600                                                                          
171700     MOVE MFS-ROER-EJ-FAELT       TO MOD-KDNOTES                          
171800                                     MOD-BEVARREF                         
171900                                                                          
172000     .                                                                    
172100     EJECT                                                                
172200* IMS SEKTIONER                                                           
172300                                                                          
172400 IMS-GET-MSG SECTION.                                                     
172500                                                                          
172600     MOVE '  QC' TO GODK-STATUSKODER                                      
172700     CALL CBLTDLI USING GU                                                
172800                          MSG-PCB                                         
172900                          MSG-IO-AREA                                     
173000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
173100     PERFORM IMS-STATUSKONTROLL                                           
173200                                                                          
173300     .                                                                    
173400 IMS-INSERT-MSG SECTION.                                                  
173500                                                                          
173600     IF ENGLISH-TEXT                                                      
173700       MOVE 'N' TO MFS-KDHUVOMR                                           
173800     END-IF                                                               
173900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
174000     MOVE SPACE TO GODK-STATUSKODER                                       
174100     CALL CBLTDLI USING ISRT                                              
174200                          MSG-PCB                                         
174300                          MSG-IO-AREA                                     
174400                          MFS-IDMOD                                       
174500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
174600     PERFORM IMS-STATUSKONTROLL                                           
174700                                                                          
174800     EJECT                                                                
174900     .                                                                    
175000 IMS-GU-WDB201 SECTION.                                                   
175100                                                                          
175200     STRING 'WDB201  (IDGMT    =' W-WDB201KEY-X ')'                       
175300            DELIMITED BY SIZE INTO SSA1                                   
175400     MOVE '  GE' TO GODK-STATUSKODER                                      
175500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB2 SSA1                 
175600     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
175700     PERFORM IMS-STATUSKONTROLL                                           
175800     .                                                                    
175900 IMS-GU-WDB401 SECTION.                                                   
176000                                                                          
176100     STRING 'WDB401  (WDB401KY =' W-WDB401KEY-X ')'                       
176200            DELIMITED BY SIZE INTO SSA1                                   
176300     MOVE '  GE' TO GODK-STATUSKODER                                      
176400     CALL CBLTDLI USING GU WDB4-PCB DLI-IO-AREA SSA1                      
176500     MOVE WDB4-STATUS-CODE TO STATUS-WS                                   
176600     PERFORM IMS-STATUSKONTROLL                                           
176700     .                                                                    
176800 IMS-GHU-WDB414 SECTION.                                                  
176900                                                                          
177000     STRING 'WDB401  (WDB401KY =' W-WDB401KEY-X ')'                       
177100            DELIMITED BY SIZE INTO SSA1                                   
177200     STRING 'WDB414  (WDB414KY =' W-WDB414KEY-X ')'                       
177300            DELIMITED BY SIZE INTO SSA2                                   
177400     MOVE '  GE' TO GODK-STATUSKODER                                      
177500     CALL CBLTDLI USING GHU WDB4-PCB DLI-IO-AREA SSA1 SSA2                
177600     MOVE WDB4-STATUS-CODE TO STATUS-WS                                   
177700     PERFORM IMS-STATUSKONTROLL                                           
177800     .                                                                    
177900     EJECT                                                                
178000 IMS-GNP-WDB414 SECTION.                                                  
178100                                                                          
178200     STRING 'WDB414  (WDB414KY =' W-WDB414KEY-X ')'                       
178300            DELIMITED BY SIZE INTO SSA1                                   
178400     MOVE '  GE' TO GODK-STATUSKODER                                      
178500     CALL CBLTDLI USING GNP WDB4-PCB DLI-IO-AREA SSA1                     
178600     MOVE WDB4-STATUS-CODE TO STATUS-WS                                   
178700     PERFORM IMS-STATUSKONTROLL                                           
178800     .                                                                    
178900 IMS-GHNP-WDB414 SECTION.                                                 
179000                                                                          
179100     STRING 'WDB414  (WDB414KY =' W-WDB414KEY-X ')'                       
179200            DELIMITED BY SIZE INTO SSA1                                   
179300     MOVE '  GE' TO GODK-STATUSKODER                                      
179400     CALL CBLTDLI USING GHNP WDB4-PCB DLI-IO-AREA SSA1                    
179500     MOVE WDB4-STATUS-CODE TO STATUS-WS                                   
179600     PERFORM IMS-STATUSKONTROLL                                           
179700                                                                          
179800     .                                                                    
179900     EJECT                                                                
180000 IMS-INSERT-WDB401 SECTION.                                               
180100                                                                          
180200     MOVE 'WDB401   ' TO SSA1                                             
180300     MOVE '  ' TO GODK-STATUSKODER                                        
180400     CALL CBLTDLI USING ISRT WDB4-PCB DLI-IO-AREA SSA1                    
180500     MOVE WDB4-STATUS-CODE TO STATUS-WS                                   
180600     PERFORM IMS-STATUSKONTROLL                                           
180700                                                                          
180800     .                                                                    
180900 IMS-DELETE-WDB414 SECTION.                                               
181000                                                                          
181100     MOVE '  ' TO GODK-STATUSKODER                                        
181200     CALL CBLTDLI USING DLET WDB4-PCB DLI-IO-AREA                         
181300     MOVE WDB4-STATUS-CODE TO STATUS-WS                                   
181400     PERFORM IMS-STATUSKONTROLL                                           
181500                                                                          
181600     .                                                                    
181700                                                                          
181800 IMS-INSERT-WDB414 SECTION.                                               
181900                                                                          
182000     MOVE 'WDB414   ' TO SSA1                                             
182100     MOVE '  ' TO GODK-STATUSKODER                                        
182200     CALL CBLTDLI USING ISRT WDB4-PCB DLI-IO-AREA SSA1                    
182300     MOVE WDB4-STATUS-CODE TO STATUS-WS                                   
182400     PERFORM IMS-STATUSKONTROLL                                           
182500     .                                                                    
182600                                                                          
182700 IMS-REPL-WDB414 SECTION.                                                 
182800                                                                          
182900     MOVE '  ' TO GODK-STATUSKODER                                        
183000     CALL CBLTDLI USING REPL WDB4-PCB DLI-IO-AREA                         
183100     MOVE WDB4-STATUS-CODE TO STATUS-WS                                   
183200     PERFORM IMS-STATUSKONTROLL                                           
183300     .                                                                    
183400     EJECT                                                                
183500                                                                          
183593 IMS-GU-WDB601-FTG SECTION.                                               
183594     STRING 'WDB601  (IDFTG    =' W-IDFTG-B6 ')'                          
183595            DELIMITED BY SIZE INTO SSA1                                   
183596     MOVE '  GE'                 TO GODK-STATUSKODER                      
183597     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-WDB601 SSA1               
183598     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
183599     PERFORM IMS-STATUSKONTROLL                                           
183600     .                                                                    
183601     SKIP3                                                                
183602                                                                          
183610 IMS-STATUSKONTROLL SECTION.                                              
183700                                                                          
183800     SET STATUS-IX TO 1                                                   
183900     SEARCH GODK-STATUS                                                   
184000       AT END                                                             
184100         CALL FELLOG                                                      
184200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
184300     END-SEARCH                                                           
184400                                                                          
184500     .                                                                    
184600     EJECT                                                                
184700*    -COPY WY2000P1                                                       
