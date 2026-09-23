000102 ID DIVISION.                                                             
000203 PROGRAM-ID.     W4035B00.                                                
000303 AUTHOR.         BERT ANDERSSON.                                          
000403 DATE-WRITTEN.   09/02/09.                                                
000503 DATE-COMPILED.                                                           
000603                                                                          
000704*    NAME:       CARPARTS.3IV2.REQUASSIGNMENT                             
000803*                                                                         
000903*        HÄMTAR ORDERDELAR TILL EN PLOCKSATS.                             
001003*        LÄSER PRC-KANAL FÖR ATT KOLLA PLOCKGRÄNS.                        
001103*        OM ENDAST EN ORDERDEL INGÅR I PLOCKSATSEN                        
001203*        HÄMTAS FÖRRÅDSDATATEXTEN.                                        
001303*        LÄGGER UPP ORDERDELAR PÅ ARBETSTABELL.                           
001403*                                                                         
001503*        PROGRAMMET ÄR EN KOPIA AV W4035100 OCH MODIFIERAT FÖR            
001603*        PICK-BY-VOICE.                                                   
001703*                                                                         
001803*    FUNCTION:                                                            
001903*                                                                         
002003*    INDATA.                                                              
002103*        TRANSACTION: W4035BU                                             
002203*        REQUEST:     W403REQU + W403ASSI                                 
002303*                                                                         
002403*    OUTDATA.                                                             
002503*        RESPONSE:    W403RESP                                            
002603*        MID     :    W4I37501                                            
002703                                                                          
002803     SKIP3                                                                
002903 ENVIRONMENT DIVISION.                                                    
003003     SKIP2                                                                
003103 DATA DIVISION.                                                           
003203     SKIP3                                                                
003303 FILE SECTION.                                                            
003403     EJECT                                                                
003503 WORKING-STORAGE SECTION.                                                 
003603 77  IDPGM                       PIC X(08)   VALUE 'W4035B00'.            
003703 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
003803 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003903 77  KDRC-DISPLAY                PIC Z(5).                                
004003 77  ORDERKO-MAX                 PIC S9(9)  VALUE +99 COMP-3.             
004103 77  EXPAND-TAB-MAX              PIC S9(9)  VALUE +99 COMP-3.             
004203                                                                          
004303 77  FILLER                      PIC X(08)   VALUE 'CURRENT'.             
004403 77  WS-CURRENT-SECTION          PIC X(32)   VALUE SPACE.                 
004503 77  FILLER                      PIC X(08)   VALUE 'IMS-SEC'.             
004603 77  WS-CURRENT-IMS-SECTION      PIC X(40)   VALUE SPACE.                 
004703                                                                          
004803*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004903*hex BB ger tecknet |                                                     
005003 77  VBAR                        PIC X       VALUE X'BB'.                 
005103 77  JA                          PIC X       VALUE 'J'.                   
005203 77  YES                         PIC X       VALUE 'Y'.                   
005303 77  NEJ                         PIC X       VALUE 'N'.                   
005403*                                                                         
005503 77  WS-CONSTANT-FLORDKNY        PIC X       VALUE 'N'.                   
005603                                                                          
005703 77  WS-MFS-KDMFSFOR             PIC X(1) VALUE SPACE.                    
005803     88                    SWEDISH-TEXT VALUE '1'.                        
005903     88                    ENGLISH-TEXT VALUE '2'.                        
006003                                                                          
006103 77  IX1                         PIC S9(9)  VALUE +0 COMP-3.              
006203 77  SPRAK-IX                    PIC S9(9)  VALUE +0 COMP-3.              
006303 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +335 COMP-3.            
006406                                                                          
006503 01      WS-KLOCKAN.                                                      
006603   03    WS-TIHHMMSS             PIC 9(6).                                
006703   03    FILLER                  PIC X(2).                                
006803                                                                          
006903 01      WS-KLOCKAN-LOK.                                                  
007003   03    WS-TIHHMMSS-LOK         PIC 9(6).                                
007103   03    FILLER                  PIC X(2).                                
007203                                                                          
007303 01      WS-DARFS                PIC 9(12).                               
007403 01      FILLER REDEFINES WS-DARFS.                                       
007503   03    FILLER                  PIC 9(2).                                
007603   03    WS-RFS-DATUM            PIC 9(6).                                
007703   03    FILLER                  PIC 9(4).                                
007803                                                                          
007903 77      WS-PRC-KVORDER          PIC S9(7)      COMP-3 VALUE 0.           
008003 77      WS-PRC-KVRADER          PIC S9(5)      COMP-3 VALUE 0.           
008103 77      WS-PRC-VKORDNTO         PIC S9(6)V9(1) COMP-3 VALUE 0.           
008203 77      WS-PRC-VLORDNTO         PIC S9(4)V9(3) COMP-3 VALUE 0.           
008303 77      WS-PRC-KVPLSRAD         PIC S9(5)      COMP-3 VALUE 0.           
008403 77      WS-PRC-VKPLSNTO         PIC S9(6)V9(1) COMP-3 VALUE 0.           
008503 77      WS-PRC-VLPLSNTO         PIC S9(4)V9(3) COMP-3 VALUE 0.           
008603 77      WS-PRC-SPLITGRANS       PIC S9(7)V9(3) COMP-3.                   
008703 77      WS-PRC-RESPLIT          PIC S9(1)V9(2) COMP-3.                   
008803 77      WS-PRC-FLSTJORD         PIC  X(1).                               
008903                                                                          
009003 77      WS-KVORDER              PIC S9(7)      COMP-3 VALUE 0.           
009103 77      WS-KVRADER              PIC S9(5)      COMP-3 VALUE 0.           
009203 77      WS-VKORDNTO             PIC S9(6)V9(1) COMP-3 VALUE 0.           
009303 77      WS-VLORDNTO             PIC S9(4)V9(3) COMP-3 VALUE 0.           
009403 77      WS-SPLITGRANS           PIC S9(7)V9(3) COMP-3.                   
009503 77      WS-SPLITREST            PIC S9(7)V9(3) COMP-3.                   
009603 77      WS-RESPLIT              PIC S9(1)V9(2) COMP-3.                   
009703 77      WS-PROC-RAD             PIC S9(8)V9    COMP-3.                   
009803 77      WS-PROC-VIKT            PIC S9(8)V9    COMP-3.                   
009903 77      WS-PROC-VOLYM           PIC S9(8)V9    COMP-3.                   
010003 77      WS-ANTSPLIT             PIC  9(8)V99.                            
010103 77      WS-KDSORT               PIC  X(2)      VALUE SPACE.              
010203 77      WS-ORQI-OHUV-FLKLAR     PIC  X(1).                               
010303                                                                          
010403 77  WS-KDMATT                   PIC X(1).                                
010503     88 US-MATT                  VALUE 'U'.                               
010603                                                                          
010703 77  WS-MINST-EN-ORDERDEL-HITTAD PIC  X(1).                               
010803                                                                          
010903 77      WS-DATUM                PIC 9(6).                                
011003 77      WS-DATUM-LOK            PIC 9(6).                                
012003                                                                          
012103*STJÄRNORDRAR                                                             
012203 01  FILLER                      PIC X(16)  VALUE 'EXPAND-KUND'.          
012303 01  W-ANTAL-MOD-RADER           PIC S9(9)  VALUE +0.                     
012403 01  EXPAND-IX                   PIC S9(9)  VALUE +0 COMP-3.              
012503 01  EXPAND-KUND-TAB.                                                     
012603     03  EXPAND-KUND OCCURS 99.                                           
012703         05  EXPAND-IDDISTR      PIC X(4).                                
012803         05  EXPAND-IDKUNDNR     PIC X(6).                                
012903         05  EXPAND-FLER         PIC X.                                   
013003                                                                          
013103*ORDERKO-TABELL                                                           
013203 01  FILLER                      PIC X(16)  VALUE 'ORDERKO-RADER'.        
013303 01  WS-ANTAL-ORDERKO-RADER      PIC S9(9)  VALUE +0.                     
013403 01  WS-ORDERKO-IX                  PIC 9(9)   VALUE 0.                   
013503 01  ORDERKO-TABELL.                                                      
013603     03 ORDERKO-LIST OCCURS 99 INDEXED BY ORDERKO-IX.                     
013703         05  ORDERKO-IDDISTR     PIC 9(4).                                
013803         05  ORDERKO-IDKUNDNR    PIC 9(6).                                
013903         05  ORDERKO-FLER        PIC X.                                   
014003                                                                          
014103*STJÄRNORDRAR                                                             
014203                                                                          
014303*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
014403*                                                                         
014503*      --- VALID IDDC CODES                                               
014603*                                                                         
014703*01    -COPY WWDC99                                                       
014803       EJECT                                                              
014903*                                                                         
015003 01      WS-IDPRC.                                                        
015103   03    WS-IDPRCBAS             PIC X(03).                               
015203   03    WS-IDPRCVAR             PIC X(01).                               
015303                                                                          
015403 01      WS-IDUSER               PIC X(08).                               
015503 01      FILLER REDEFINES WS-IDUSER.                                      
015603   03    WS-IDUSER-LEADING-ZERO  PIC 9(03).                               
015703   03    WS-REQU-IDANSTNR        PIC 9(05).                               
015803                                                                          
015903*    -- BORD NOT USED BY PICK-BY-VOICE, USE FIXED VALUE SPACE             
016003 01  WS-IDBORD                   PIC X(03)   VALUE SPACE.                 
016103                                                                          
016203 77  INDATA-SW                   PIC X       VALUE 'J'.                   
016303     88  INDATA-OK                           VALUE 'J'.                   
016403     88  INDATA-FEL                          VALUE 'N'.                   
016503                                                                          
016603 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
016703     88  NYCKLAR-OK                          VALUE 'J'.                   
016803     88  NYCKLAR-FEL                         VALUE 'N'.                   
016903                                                                          
017003 77  PRC-FEL-SW                  PIC X       VALUE 'J'.                   
017103     88  PRC-FEL                             VALUE 'N'.                   
017203                                                                          
017303 77  ALLT-SW                     PIC X       VALUE 'J'.                   
017403     88  ALLT-OK                             VALUE 'J'.                   
017503                                                                          
017603 77  PLOCKGRANS-SW               PIC X       VALUE 'N'.                   
017703     88  PLOCKGRANS                          VALUE 'J'.                   
017803                                                                          
017903 77  SPLIT-SW                    PIC X       VALUE 'N'.                   
018003     88  SPLIT                               VALUE 'J'.                   
018103     88  EJ-SPLIT                            VALUE 'N'.                   
018203                                                                          
018303 77  ORDERDEL-SW                 PIC X       VALUE 'N'.                   
018403     88 ORDERDEL-FINNS                       VALUE 'N'.                   
018503     88 ORDERDEL-SAKNAS                      VALUE 'J'.                   
018603                                                                          
018703 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
018803     88  OMSTART                             VALUE 'J'.                   
018903                                                                          
019003 77  FORSTA-SW                   PIC X       VALUE 'J'.                   
019103     88  FORSTA-GANG                         VALUE 'J'.                   
019203                                                                          
019303 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
019403     88  FIRST-TIME                          VALUE 'J'.                   
019503                                                                          
019603 77  PRC-BRYT-SW                 PIC X       VALUE 'N'.                   
019703     88  PRC-BRYTNING                        VALUE 'J'.                   
019803                                                                          
019903 77  LAES-SW                     PIC X       VALUE 'N'.                   
020003     88  LAES-OK                             VALUE 'J'.                   
020103                                                                          
020203*STJÄRNORDRAR                                                             
020303                                                                          
020403 77  VARIANT-KOD          PIC X.                                          
020503     88 VARIANT-PLOCKGRANS        VALUE '1'.                              
020603     88 VARIANT-STJARNORDER       VALUE '2'.                              
020703     88 VARIANT-UTAN-STJARNORDER  VALUE '3'.                              
020803                                                                          
020903 77  TA-MED-ORDERDEL-SW   PIC X   VALUE 'J'.                              
021003     88 TA-MED-ORDERDEL           VALUE 'J'.                              
021103                                                                          
021203 77  BRYTA-SW             PIC X   VALUE 'N'.                              
021303     88 BRYTA                     VALUE 'J'.                              
021403     88 BRYTA-NEJ                 VALUE 'N'.                              
021503                                                                          
021603 77  DUBLETT-PA-FORSTA-OD-SAKNAS-SW  PIC X   VALUE 'J'.                   
021703     88 DUBLETT-PA-FORSTA-OD-SAKNAS          VALUE 'N'.                   
021803                                                                          
021903*STJÄRNORDRAR                                                             
022003                                                                          
022103 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
022203     88  EGEN-MID                            VALUE '4351'.                
022303     88  GODK-MID                            VALUE '4351'.                
022403     EJECT                                                                
022503*                                                                         
022603                                                                          
022703 77  REQUEST-SW                  PIC X       VALUE 'J'.                   
022803     88  REQUEST-OK                          VALUE 'J'.                   
022903     88  REQUEST-WRONG                       VALUE 'N'.                   
023003                                                                          
023103 01  MESSAGE-CODES.                                                       
023203     03  ERR-ORDERDELAR-SLUT     PIC X(3)    VALUE '035'.                 
023303     03  ERR-ORDERDELAR-SAKNAS   PIC X(3)    VALUE '036'.                 
023403     03  ERR-PRC-SAKNAS          PIC X(3)    VALUE '073'.                 
023503     03  ERR-INVALID-USER        PIC X(3)    VALUE '405'.                 
023603     03  ERR-INVALID-DC          PIC X(3)    VALUE '440'.                 
023703     03  ERR-INVALID-SERIAL-NO   PIC X(3)    VALUE '491'.                 
023803     03  ERR-INVALID-VALUE       PIC X(3)    VALUE '492'.                 
023903     03  ERR-VALUE-TOO-LONG      PIC X(3)    VALUE '493'.                 
024003     03  ERR-WRONG-RCD-TYPE      PIC X(3)    VALUE '495'.                 
024103     03  ERR-WRONG-MSG-TYPE      PIC X(3)    VALUE '497'.                 
024203     03  ERR-INV-MSG-STRUCTURE   PIC X(3)    VALUE '499'.                 
024303     EJECT                                                                
024403*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
024503 01  GENERAL-SUBPROGRAMS.                                                 
024603     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
024703     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
024803     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
024903     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
025003     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
025103     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
025203     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
025303     SKIP3                                                                
025403*    --- PARAMETERS TO ABEND                                              
025503 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
025603 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
025703 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
025803                                                                          
025903 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
026003     EJECT                                                                
026103*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
026203*01  -COPY WMSGINIT                                                       
026303                                                                          
026403*    --- PARAMETERS TO WMEDKONV                                           
026503*01  -COPY WMEDAREA                                                       
026603                                                                          
026703*    -- WORK FIELD FOR EDITING WMEDKONV MESSAGE                           
026803 01  TEMP-MESSAGE                PIC X(50).                               
026903     EJECT                                                                
027003*    --- PARAMETERS TO WZ01RECV                                           
027103 01  FILLER                      PIC X(16)   VALUE 'WZ01RECV'.            
027203     SKIP3                                                                
027303*01  -COPY WZ01RECV                                                       
027403     EJECT                                                                
027503*    --- PARAMETERS TO WZ01SEND                                           
027603 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
027703     SKIP3                                                                
027803*01  -COPY WZ01SEND                                                       
027903     EJECT                                                                
028003*                                                                         
028103 01  FILLER                      PIC X(16)   VALUE 'RECV AREA'.           
028203     SKIP3                                                                
028303 01  RECV-AREA                   PIC X(1000).                             
028403     SKIP3                                                                
028503 01  FILLER                      PIC X(16)   VALUE 'REQUEST HDR'.         
028603     SKIP3                                                                
028703*01  -COPY W403REQU                                                       
028803     SKIP3                                                                
028903*01  -COPY W403ASSI                                                       
029003     EJECT                                                                
029103 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
029203     SKIP3                                                                
029303 01  SEND-AREA                   PIC X(1000).                             
029403     SKIP3                                                                
029503 01  FILLER                      PIC X(16)   VALUE 'RESPONSE HDR'.        
029603     SKIP3                                                                
029703*01  -COPY W403RESP                                                       
029803     EJECT                                                                
029903*    --- WORK-AREAS FOR MESSAGE PROCESSING                                
030003*                                                                         
030103 01  FILLER                      PIC X(16)   VALUE 'MSG-WS'.              
030203     SKIP3                                                                
030303 01  FIELD-LENGTHS.                                                       
030403*    --- W403REQU FIELDS                                                  
030503     03 LIDMSG3IV                PIC S9(4)   BINARY.                      
030603     03 LIDMVER3IV               PIC S9(4)   BINARY.                      
030703     03 LIDMTYP3IV               PIC S9(4)   BINARY.                      
030803     03 LIDDC                    PIC S9(4)   BINARY.                      
030903     03 LIDANSTNR                PIC S9(4)   BINARY.                      
031003     03 LIDSNO3IV                PIC S9(4)   BINARY.                      
031103     03 LIDRTYP3IV               PIC S9(4)   BINARY.                      
031203     03 LIDPRC                   PIC S9(4)   BINARY.                      
031303                                                                          
031403*    --- W403RESP FIELDS                                                  
031503     03 LTISTAMP3IV              PIC S9(4)   BINARY.                      
031603     03 LKDRESP3IV               PIC S9(4)   BINARY.                      
031703     03 LBERESP3IV               PIC S9(4)   BINARY.                      
031803                                                                          
031903     03 LIDPRODNR                PIC S9(4)   BINARY.                      
032003                                                                          
032103     EJECT                                                                
032203 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW1'.          
032303 01  P-TO-P-SW1.                                                          
032403     03  PTOP1-LL                PIC S9(4)   VALUE 28 COMP SYNC.          
032503     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
032603     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
032703     03  PTOP1-TRANSKOD          PIC  X(7)   VALUE 'W4T375U'.             
032803     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
032903     03  FILLER                  PIC  X(4)   VALUE '435B'.                
033003     03  PTOP1-KDMFSFOR          PIC  X(1).                               
033103*    03  -COPY W4I37501  -PRE PTOP1-                                      
033203                                                                          
033303*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
033403*                                                                         
033503 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
033603     SKIP3                                                                
033703 01  NYCKLAR-TILL-DLI.                                                    
033803                                                                          
033903*----> PLOCKSATS                                                          
034003                                                                          
034103     03  W-4001-IDHTYP-X.                                                 
034203         05  W-4001-IDHTYP           PIC  X(4)  VALUE '4001'.             
034303         05  W-4001-IDPRODNR         PIC  9(7).                           
034403         05  W-4001-IDPLKLST         PIC  9(3).                           
034503         05  FILLER                  PIC  X(16) VALUE LOW-VALUE.          
034603                                                                          
034703*----> DIREKTNYCKEL ORDERHUVUD.                                           
034803                                                                          
034903     03  W-IDORDER-X.                                                     
035003         05  W-IDORDER               PIC S9(7)  COMP-3.                   
035103                                                                          
035203     03  W-IDDC-X.                                                        
035303         05  W-IDDC                  PIC X(2).                            
035403                                                                          
035503*----> DIREKTNYCKEL TILL ORDERDEL.                                        
035603                                                                          
035703     03  W-WDQ301KY-X.                                                    
035803         05  W-Q301KY-IDORDER        PIC S9(7)  COMP-3.                   
035903         05  W-Q301KY-IDDC           PIC X(2).                            
036003         05  W-Q301KY-IDPRODNR       PIC S9(7)  COMP-3.                   
036103         05  W-Q301KY-IDPLKLST       PIC S9(3)  COMP-3.                   
036203                                                                          
036303                                                                          
036403*----> SEKUNDÄR INDEX TILL ORDERDEL.                                      
036503                                                                          
036603     03  W-WDQ3BSEQ-MIN-X.                                                
036703         05  W-Q3BSEQ-MIN-IDDC       PIC X(2).                            
036803         05  W-Q3BSEQ-MIN-IDPRCBAS   PIC X(3).                            
036903         05  W-Q3BSEQ-MIN-DAUTSKR    PIC  9(8).                           
037003         05  W-Q3BSEQ-MIN-TIUTSTID   PIC S9(7)  COMP-3.                   
037103         05  W-Q3BSEQ-MIN-DARFS      PIC  9(12).                          
037203         05  W-Q3BSEQ-MIN-DALSTORD   PIC  9(12).                          
037303         05  FILLER                  PIC X(1).                            
037403                                                                          
037503     03      W-Q3BSEQ-MIN-IDPRCVAR   PIC X(1).                            
037603                                                                          
037703     03  W-WDQ3BSEQ-MAX-X.                                                
037803         05  W-Q3BSEQ-MAX-IDDC       PIC X(2).                            
037903         05  W-Q3BSEQ-MAX-IDPRCBAS   PIC X(3).                            
038003         05  W-Q3BSEQ-MAX-DAUTSKR    PIC  9(8).                           
038103         05  W-Q3BSEQ-MAX-TIUTSTID   PIC S9(7)  COMP-3.                   
038203         05  W-Q3BSEQ-MAX-DARFS      PIC  9(12).                          
038303         05  W-Q3BSEQ-MAX-DALSTORD   PIC  9(12).                          
038403         05  FILLER                  PIC X(1).                            
038503                                                                          
038603     03      W-Q3BSEQ-MAX-IDPRCVAR   PIC X(1).                            
038703                                                                          
038803*----> SEKUNDÄR INDEX PRC-KANAL TILL ORDERDEL.                            
038903                                                                          
039003     03  W-WDQ3B1-X.                                                      
039103         05  W-Q3B1-IDDC             PIC X(2).                            
039203         05  W-Q3B1-IDPRCBAS         PIC X(3).                            
039303         05  FILLER                  PIC X(47).                           
039403                                                                          
039503     03      W-Q3B1-IDPRCVAR         PIC X(1).                            
039603                                                                          
039703     03  W-WDQ3B1-MIN-X.                                                  
039803         05  W-Q3B1-MIN-IDDC         PIC X(2).                            
039903         05  W-Q3B1-MIN-IDPRCBAS     PIC X(3).                            
040003         05  W-Q3B1-MIN-DAUTSKR      PIC 9(8).                            
040103         05  W-Q3B1-MIN-TIUTSTID     PIC S9(7)  COMP-3.                   
040203         05  W-Q3B1-MIN-DARFS        PIC 9(12).                           
040303         05  W-Q3B1-MIN-DALSTORD     PIC 9(12).                           
040403         05  FILLER                  PIC X.                               
040503         05  FILLER                  PIC X(10).                           
040603                                                                          
040703     03      W-Q3B1-MIN-IDPRCVAR     PIC X(1).                            
040803                                                                          
040903     03  W-WDQ3B1-MAX-X.                                                  
041003         05  W-Q3B1-MAX-IDDC         PIC X(2).                            
041103         05  W-Q3B1-MAX-IDPRCBAS     PIC X(3).                            
041203         05  W-Q3B1-MAX-DAUTSKR      PIC 9(8).                            
041303         05  W-Q3B1-MAX-TIUTSTID     PIC S9(7)  COMP-3.                   
041403         05  W-Q3B1-MAX-DARFS        PIC 9(12).                           
041503         05  W-Q3B1-MAX-DALSTORD     PIC 9(12).                           
041603         05  FILLER                  PIC X.                               
041703         05  FILLER                  PIC X(10).                           
041803                                                                          
041903     03      W-Q3B1-MAX-IDPRCVAR     PIC X(1).                            
042003                                                                          
042103                                                                          
042203*----> PRC-KANALEN.                                                       
042303                                                                          
042403     03  W-4447-IDHTYP-X.                                                 
042503         05  W-4447-IDHTYP       PIC  X(04) VALUE '4447'.                 
042603         05  W-4447-IDDC         PIC  X(02).                              
042703         05  W-4447-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
042803                                                                          
042903     03  W-4448-IDPRC-X.                                                  
043003         05  W-4448-IDPRC        PIC  X(04).                              
043103         05  W-4448-LOW-VALUE    PIC  X(01) VALUE LOW-VALUE.              
043203                                                                          
043303*----> FÖRRÅDSDATATEXT.                                                   
043403                                                                          
043503     03  W-4535-IDHTYP-X.                                                 
043603         05  W-4535-IDHTYP       PIC  X(04) VALUE '4535'.                 
043703         05  W-4535-KDFDKRAV     PIC S9(03) COMP-3.                       
043803         05  W-4535-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
043903                                                                          
044003     03  W-4536-IDSKYLT-X.                                                
044103         05  W-4536-IDSKYLT      PIC  X(03).                              
044203         05  W-4536-LOW-VALUE    PIC  X(02) VALUE LOW-VALUE.              
044303                                                                          
044403*----> PLOCKSATSENS LÖPNR INOM PRCGRUPP.                                  
044503                                                                          
044603     03  W-4461-IDHTYP-X.                                                 
044703         05  W-4461-IDHTYP       PIC  X(04) VALUE '4461'.                 
044803         05  W-4461-IDDC         PIC  X(02).                              
044903         05  W-4461-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
045003                                                                          
045103     03  W-4462-KDPRCGRP-X       PIC  X(05).                              
045203                                                                          
045303*                                                                         
045403 01  FILLER                      PIC X(16)  VALUE 'SPAR-AREA  '.          
045503 01  SPAR-AREA.                                                           
045603     03  SPAR-IDTRANS            PIC X(4).                                
045703     03  SPAR-IDDISTR-X.                                                  
045803         05  SPAR-IDDISTR        PIC S9(5)  COMP-3.                       
045903     03  SPAR-IDKUNDNR-X.                                                 
046003         05  SPAR-IDKUNDNR       PIC S9(7)  COMP-3.                       
046103*                                                                         
046203*    --- STATUS-KOD FRÅN IMS                                              
046303 01  STATUS-ORDERDEL-Q3B1-WS     PIC X(02).                               
046403     88    ORDERDEL-Q3B1-FINNS               VALUE '  '.                  
046503     88    ORDERDEL-Q3B1-SAKNAS              VALUE 'GE' 'GB'.             
046603 01  STATUS-WS                   PIC XX.                                  
046703     88  SEGMENT-FINNS                       VALUE '  '.                  
046803     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
046903     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
047003     88  END-OF-DATA                         VALUE 'GB'.                  
047103     SKIP2                                                                
047203 01  FILLER                      PIC X(08)  VALUE 'SSA-AREA'.             
047303 01  GODK-STATUSKODER.                                                    
047403     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
047503     SKIP3                                                                
047603 01  SSA1                        PIC X(200).                              
047703 01  SSA2                        PIC X(160).                              
047803     EJECT                                                                
047903****************************                                              
048003*  ARBETSAREA FÖR ORDERDEL *                                              
048103****************************                                              
048203*01  W-ORDERDEL.                                                          
048303*    03   COPY WDQ301     -PRE W-                                         
048403*    EJECT                                                                
048503*    --- IMS FUNKTIONSKODER                                               
048603*01  -COPY W0003                                                          
048703     EJECT                                                                
048803*    ---  DLI INPUT-OUTPUT AREA                                           
048903 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
049003     SKIP3                                                                
049103 01  DLI-IO-AREA.                                                         
049203     03  IO-AREA                 PIC X(1600) VALUE SPACE.                 
049303     SKIP3                                                                
049403     03  WLXXKH01 REDEFINES IO-AREA.                                      
049503*        05  -COPY WDGX4447   -PRE XXKH-                                  
049603     EJECT                                                                
049703     03  WLXXKH11 REDEFINES IO-AREA.                                      
049803*        05  -COPY WDGX4448   -PRE XXKH-                                  
049903     EJECT                                                                
050003     03  WLXXKQ01 REDEFINES IO-AREA.                                      
050103*        05  -COPY WDGX01     -PRE XXKQ-                                  
050203     EJECT                                                                
050303     03  WLXXKU01 REDEFINES IO-AREA.                                      
050403*        05  -COPY WDGX4535   -PRE XXKU-                                  
050503     EJECT                                                                
050603     03  WLXXKU11 REDEFINES IO-AREA.                                      
050703*        05  -COPY WDGX4536   -PRE XXKU-                                  
050803     EJECT                                                                
050903     03  WLXXKO01 REDEFINES IO-AREA.                                      
051003*        05  -COPY WDGX4461   -PRE XXKO-                                  
051103     EJECT                                                                
051203     03  WLXXKO11 REDEFINES IO-AREA.                                      
051303*        05  -COPY WDGX4462   -PRE XXKO-                                  
051403     EJECT                                                                
051503     03  WL400101 REDEFINES IO-AREA.                                      
051603*        05  -COPY WDGX4001   -PRE 4001-                                  
051703     EJECT                                                                
051803 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
051903 01  DLI-IO-AREA1.                                                        
052003     03  WLORQA01.                                                        
052103*        05  -COPY WDQ301     -PRE ORQA-                                  
052203     EJECT                                                                
052303 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
052403 01  DLI-IO-AREA2.                                                        
052503*    03 -COPY WDQ201     -PRE ORQI-                                       
052603*    03 -COPY WDQ212     -PRE ORQI-                                       
052703     EJECT                                                                
052803 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
052903 01  DLI-IO-AREA3.                                                        
053003     03 WL400111.                                                         
053103*        05 -COPY WDGX4002                                                
053203     EJECT                                                                
053303 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q3B1'.         
053403 01  DLI-IO-Q3B1.                                                         
053503     03  DLI-IO-WDQ3B1.                                                   
053603*        05  -COPY WDQ3B1   -PRE Q3B1-                                    
053703     EJECT                                                                
053803                                                                          
053903 LINKAGE SECTION.                                                         
054003                                                                          
054103*01  -COPY W0009      -PRE MSG-                                           
054203     EJECT                                                                
054303*01  -COPY W0009      -PRE ALT1-                                          
054403     EJECT                                                                
054503*01  -COPY W0008      -PRE USEA-                                          
054603     05  FILLER                  PIC X.                                   
054703     EJECT                                                                
054803*01  -COPY W0008      -PRE ORQA-                                          
054903     05  FILLER                  PIC X.                                   
055003     EJECT                                                                
055103*01  -COPY W0008      -PRE ORQB-                                          
055203     05  FILLER                  PIC X.                                   
055303     EJECT                                                                
055403*01  -COPY W0008      -PRE XXKH-                                          
055503     05  FILLER                  PIC X.                                   
055603     EJECT                                                                
055703*01  -COPY W0008      -PRE 4001-                                          
055803     05  FILLER                  PIC X.                                   
055903     EJECT                                                                
056003*01  -COPY W0008      -PRE XXKU-                                          
056103     05  FILLER                  PIC X.                                   
056203     EJECT                                                                
056303*01  -COPY W0008      -PRE XXKO-                                          
056403     05  FILLER                  PIC X.                                   
056503     EJECT                                                                
056603*01  -COPY W0008      -PRE ORQI-                                          
056703     05  FILLER                  PIC X.                                   
056803     EJECT                                                                
056903*01  -COPY W0008      -PRE Q3B1-                                          
057003     05  FILLER                  PIC X.                                   
057103     EJECT                                                                
057203 PROCEDURE DIVISION  USING MSG-PCB                                        
057303                           ALT1-PCB                                       
057403                           USEA-PCB                                       
057503                           ORQA-PCB                                       
057603                           ORQB-PCB                                       
057703                           XXKH-PCB                                       
057803                           4001-PCB                                       
057903                           XXKU-PCB                                       
058003                           XXKO-PCB                                       
058103                           ORQI-PCB                                       
058203                           Q3B1-PCB.                                      
058303                                                                          
058403     ENTRY 'DLITCBL' USING MSG-PCB                                        
058503                           ALT1-PCB                                       
058603                           USEA-PCB                                       
058703                           ORQA-PCB                                       
058803                           ORQB-PCB                                       
058903                           XXKH-PCB                                       
059003                           4001-PCB                                       
059103                           XXKU-PCB                                       
059203                           XXKO-PCB                                       
059303                           ORQI-PCB                                       
059403                           Q3B1-PCB.                                      
059503                                                                          
059603 MAIN SECTION.                                                            
059703                                                                          
059803     PERFORM S03-RECEIVE-OPEN                                             
059903     IF RECV-KDRC = 0                                                     
060003       PERFORM A-INIT                                                     
060103       PERFORM B-PROCESS-REQUEST                                          
060203       IF REQUEST-OK                                                      
060303*STJÄRNORDRAR                                                             
060403         PERFORM F-LAES-PRC-KANAL                                         
060503         IF SEGMENT-SAKNAS                                                
060603           MOVE SPACE     TO TEMP-MESSAGE                                 
060703           MOVE ERR-PRC-SAKNAS TO MED-IDMFSFEL                            
060803           CALL WMEDKONV USING MED-WMEDAREA                               
060903           SET REQUEST-WRONG TO TRUE                                      
061003         ELSE                                                             
061103                                                                          
061203           IF XXKH-4448-FLSTJORD = JA                                     
061303             SET VARIANT-STJARNORDER TO TRUE                              
061403             PERFORM G-KOLLA-FORSTA-ORDERDEL                              
061503                                                                          
061603             IF  WS-MINST-EN-ORDERDEL-HITTAD = JA                         
061703             AND DUBLETT-PA-FORSTA-OD-SAKNAS                              
061803               SET VARIANT-UTAN-STJARNORDER TO TRUE                       
061903               PERFORM H-SKAPA-ORDERKO-TABELL                             
062003             END-IF                                                       
062103                                                                          
062203           ELSE                                                           
062303             SET VARIANT-PLOCKGRANS TO TRUE                               
062403             PERFORM I-KOLLA-FORSTA-ORDERDEL                              
062503           END-IF                                                         
062603*STJÄRNORDRAR                                                             
062703                                                                          
062803           IF WS-MINST-EN-ORDERDEL-HITTAD = JA                            
062903             PERFORM D-INIT-PLOCKSATSREG                                  
063003             PERFORM E-SKAPA-URVAL-PLK-LST                                
064003           ELSE                                                           
064103                                                                          
064203              MOVE SPACE         TO TEMP-MESSAGE                          
064303              MOVE ERR-ORDERDELAR-SAKNAS  TO MED-IDMFSFEL                 
064403              CALL WMEDKONV USING MED-WMEDAREA                            
064503              SET REQUEST-WRONG TO TRUE                                   
064603           END-IF                                                         
064703           IF REQUEST-WRONG                                               
064803             MOVE MED-IDMFSFEL      TO RESP-KDRESP3IV                     
064903             MOVE MED-TEMFSFEL      TO RESP-BERESP3IV                     
065003           END-IF                                                         
065103         END-IF                                                           
065203       END-IF                                                             
065204       PERFORM S03-RECEIVE-CLOSE                                          
065303                                                                          
065403       IF REQUEST-WRONG                                                   
065503         PERFORM S04-SEND-OPEN                                            
065603         PERFORM C-PROCESS-RESPONSE                                       
065703         PERFORM S04-SEND-CLOSE                                           
065803       END-IF                                                             
065903     END-IF                                                               
066003                                                                          
066203                                                                          
066303     PERFORM Z-FINIT                                                      
066403     MOVE ZERO TO RETURN-CODE                                             
066503     GOBACK                                                               
066603     .                                                                    
066703     EJECT                                                                
066803 A-INIT SECTION.                                                          
066903     MOVE 'A-INIT         '   TO WS-CURRENT-SECTION                       
067003                                                                          
067103     MOVE "S  "             TO MED-IDSKYLT                                
067203     MOVE "1"               TO WS-MFS-KDMFSFOR                            
067303                                                                          
067403     MOVE ZERO              TO RESP-KDRESP3IV                             
067503     MOVE SPACE             TO RESP-BERESP3IV                             
067603     MOVE SPACE             TO TEMP-MESSAGE                               
067703     MOVE ZERO              TO WS-IDUSER-LEADING-ZERO                     
067803                                                                          
067903     MOVE SPACE             TO SPAR-IDTRANS                               
068003     MOVE ZERO              TO SPAR-IDDISTR                               
068103     MOVE ZERO              TO SPAR-IDKUNDNR                              
068203     MOVE ZERO              TO WS-ANTAL-ORDERKO-RADER                     
068303                                                                          
068403     MOVE NEJ               TO PTOP1-MID-FLSVAR                           
068503     MOVE '1'               TO PTOP1-KDMFSFOR                             
068603                                                                          
068703     MOVE NEJ               TO WS-CONSTANT-FLORDKNY                       
068803     MOVE NEJ               TO ORDERDEL-SW                                
068903     MOVE JA                TO FORSTA-SW                                  
069003                                                                          
069103     MOVE +1 TO EXPAND-IX                                                 
069203     PERFORM UNTIL EXPAND-IX > EXPAND-TAB-MAX                             
069303        MOVE SPACE TO EXPAND-IDDISTR (EXPAND-IX)                          
069403        MOVE SPACE TO EXPAND-IDKUNDNR (EXPAND-IX)                         
069503        MOVE SPACE TO EXPAND-FLER  (EXPAND-IX)                            
069603        ADD +1 TO EXPAND-IX                                               
069703     END-PERFORM                                                          
069803                                                                          
069903     .                                                                    
070003     EJECT                                                                
070103 B-PROCESS-REQUEST SECTION.                                               
070203     MOVE 'B-PROCESS-REQU ' TO WS-CURRENT-SECTION                         
070303                                                                          
070403*    -- SAVE VCOM SENDER TAG INFO FOR MQ/WMDB/VCOM GATEWAY                
070503     MOVE RECV-ADDISPXTRA TO SEND-ADDISPXTRA                              
070603                                                                          
070703     MOVE JA         TO REQUEST-SW                                        
070803     MOVE JA         TO NYCKLAR-SW                                        
070903                                                                          
071003     MOVE LOW-VALUE  TO W-WDQ3BSEQ-MIN-X                                  
071103                        W-Q3BSEQ-MIN-IDPRCVAR                             
071203                        W-WDQ3B1-X                                        
071303                                                                          
071403     MOVE HIGH-VALUE TO W-WDQ3BSEQ-MAX-X                                  
071503                        W-Q3BSEQ-MAX-IDPRCVAR                             
071603*                                                                         
071703     MOVE LOW-VALUE  TO W-WDQ3B1-MIN-X                                    
071803                        W-Q3B1-MIN-IDPRCVAR                               
071903                                                                          
072003     MOVE HIGH-VALUE TO W-WDQ3B1-MAX-X                                    
072103                        W-Q3B1-MAX-IDPRCVAR                               
072203*                                                                         
072303     PERFORM S03-RECEIVE-MESSAGE                                          
072403     IF RECV-KDRC > 0                                                     
072503*      -- HEADER MISSING                                                  
072603       MOVE ERR-INV-MSG-STRUCTURE TO MED-IDMFSFEL                         
072703       CALL WMEDKONV USING MED-WMEDAREA                                   
072803       SET REQUEST-WRONG TO TRUE                                          
072903     ELSE                                                                 
073003       PERFORM BA-PROCESS-REQUEST-HEADER                                  
073103     END-IF                                                               
073203                                                                          
073303     IF REQUEST-OK                                                        
073403        PERFORM BC-GET-LOCAL-DATE-AND-TIME                                
073503     END-IF                                                               
073603                                                                          
073703     IF REQUEST-OK                                                        
073803       PERFORM S03-RECEIVE-MESSAGE                                        
073903       IF RECV-KDRC > 0                                                   
074003*      -- PRINT REQUEST MISSING                                           
074103         MOVE ERR-INV-MSG-STRUCTURE TO MED-IDMFSFEL                       
074203         CALL WMEDKONV USING MED-WMEDAREA                                 
074303         SET REQUEST-WRONG TO TRUE                                        
074403       ELSE                                                               
074503         PERFORM BB-PROCESS-PRINT-REQUEST-DATA                            
074603       END-IF                                                             
074703     END-IF                                                               
074803                                                                          
074903     IF REQUEST-WRONG                                                     
075003       MOVE MED-IDMFSFEL  TO RESP-KDRESP3IV                               
075103       MOVE MED-TEMFSFEL  TO RESP-BERESP3IV                               
075203     END-IF                                                               
075303     .                                                                    
075403     EJECT                                                                
075503 BA-PROCESS-REQUEST-HEADER SECTION.                                       
075603     MOVE 'BA-PROCESS-REQUEST-HEADER ' TO WS-CURRENT-SECTION              
075703                                                                          
075803     UNSTRING RECV-AREA (1:RECV-KVDLEN)                                   
075903       DELIMITED BY VBAR                                                  
075904       INTO                                                               
076003         REQU-IDMSG3IV      COUNT IN LIDMSG3IV                            
076103         REQU-IDMVER3IV     COUNT IN LIDMVER3IV                           
076203         REQU-IDMTYP3IV     COUNT IN LIDMTYP3IV                           
076303         REQU-TISTAMP3IV    COUNT IN LTISTAMP3IV                          
076403         REQU-IDDC          COUNT IN LIDDC                                
076503         REQU-IDANSTNR      COUNT IN LIDANSTNR                            
076603         REQU-IDSNO3IV      COUNT IN LIDSNO3IV                            
076703       OVERFLOW                                                           
076803*      -- TOO MANY FIELDS                                                 
076903         MOVE ERR-INV-MSG-STRUCTURE TO MED-IDMFSFEL                       
077003         CALL WMEDKONV USING MED-WMEDAREA                                 
077103         SET REQUEST-WRONG TO TRUE                                        
077203     END-UNSTRING                                                         
077303                                                                          
077403     IF REQUEST-OK                                                        
077503       IF LIDMSG3IV > LENGTH OF REQU-IDMSG3IV                             
077603*        -- MSG ID TOO LONG                                               
077703         MOVE SPACE              TO TEMP-MESSAGE                          
077803         MOVE ERR-VALUE-TOO-LONG TO MED-IDMFSFEL                          
077903         CALL WMEDKONV USING MED-WMEDAREA                                 
078003         STRING 'MEDDELANDE-ID ' MED-TEMFSFEL                             
078103           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
078203         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
078303         SET REQUEST-WRONG TO TRUE                                        
078403       ELSE                                                               
078503         IF REQU-IDMSG3IV = SPACE                                         
078603         OR REQU-IDMSG3IV = LOW-VALUE                                     
078703*          -- MSG ID WRONG                                                
078803           MOVE SPACE                  TO TEMP-MESSAGE                    
078903           MOVE ERR-INVALID-VALUE      TO MED-IDMFSFEL                    
079003           CALL WMEDKONV USING MED-WMEDAREA                               
079103           STRING 'MEDDELANDE-ID ' MED-TEMFSFEL                           
079203             DELIMITED BY SIZE INTO TEMP-MESSAGE                          
079303           MOVE TEMP-MESSAGE           TO MED-TEMFSFEL                    
079403           SET REQUEST-WRONG           TO TRUE                            
079503         END-IF                                                           
079603       END-IF                                                             
079703     END-IF                                                               
079803*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE HEADER                 
079903     MOVE REQU-IDMSG3IV TO RESP-IDMSG3IV                                  
080003                                                                          
080103*--  REQU-IDMVER3IV IGNORED                                               
080203                                                                          
080303                                                                          
080403*--  REQU-IDMTYP3IV                                                       
080503                                                                          
080603     IF REQUEST-OK                                                        
080703       IF REQU-IDMTYP3IV NOT = 'RequestAssignment'                        
080803         MOVE ERR-WRONG-MSG-TYPE TO MED-IDMFSFEL                          
080903         CALL WMEDKONV USING MED-WMEDAREA                                 
081003         SET REQUEST-WRONG TO TRUE                                        
081103       END-IF                                                             
081203     END-IF                                                               
081303                                                                          
081403*--  REQU-TISTAMP3IV IGNORED                                              
081503                                                                          
081603                                                                          
081703*--  REQU-IDDC                                                            
081803                                                                          
081903     IF REQUEST-OK                                                        
082003       IF LIDDC > LENGTH OF REQU-IDDC                                     
082103       OR REQU-IDDC NOT = '11'                                            
082203*        --- INVALID DC                                                   
082303         MOVE SPACE              TO TEMP-MESSAGE                          
082403         MOVE ERR-INVALID-DC     TO MED-IDMFSFEL                          
082503         CALL WMEDKONV USING MED-WMEDAREA                                 
082603         SET REQUEST-WRONG TO TRUE                                        
082703       END-IF                                                             
082803     END-IF                                                               
082903*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE HEADER                 
083003     MOVE REQU-IDDC     TO RESP-IDDC                                      
083103                                                                          
083203*--  REQU-IDANSTNR                                                        
083303                                                                          
083403     IF REQUEST-OK                                                        
083503       IF LIDANSTNR > LENGTH OF REQU-IDANSTNR                             
083603       OR REQU-IDANSTNR NOT NUMERIC                                       
083703       OR REQU-IDANSTNR = 0                                               
083803*        --- INVALID OPERATOR ID                                          
083903         MOVE SPACE              TO TEMP-MESSAGE                          
084003         MOVE ERR-INVALID-VALUE TO MED-IDMFSFEL                           
084103         CALL WMEDKONV USING MED-WMEDAREA                                 
084203         STRING 'PACKAREIDENTITET ' MED-TEMFSFEL                          
084303           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
084403         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
084503         SET REQUEST-WRONG TO TRUE                                        
084603       ELSE                                                               
084703         MOVE REQU-IDANSTNR      TO WS-REQU-IDANSTNR                      
084803       END-IF                                                             
084903     END-IF                                                               
085003*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE HEADER                 
085103     MOVE REQU-IDANSTNR TO RESP-IDANSTNR                                  
085203                                                                          
085303*--  REQU-IDSNO3IV                                                        
085403                                                                          
085503     IF REQUEST-OK                                                        
085603       IF LIDSNO3IV > LENGTH OF REQU-IDSNO3IV                             
085703*        --- SERIAL NBR TOO LONG                                          
085803         MOVE SPACE              TO TEMP-MESSAGE                          
085903         MOVE ERR-VALUE-TOO-LONG TO MED-IDMFSFEL                          
086003         CALL WMEDKONV USING MED-WMEDAREA                                 
086103         STRING 'SERIENUMMER '  MED-TEMFSFEL                              
086203           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
086303         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
086403         SET REQUEST-WRONG TO TRUE                                        
086503       END-IF                                                             
086603     END-IF                                                               
086703*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE HEADER                 
086803     MOVE REQU-IDSNO3IV TO RESP-IDSNO3IV                                  
086903                                                                          
087003     .                                                                    
087103     EJECT                                                                
087203 BB-PROCESS-PRINT-REQUEST-DATA SECTION.                                   
087303     MOVE 'BB-PROCESS-PRINT-REQUEST-DATA'  TO WS-CURRENT-SECTION          
087403                                                                          
087503*    -- EXTRACT AND CHECK THE FIELDS IN THE "REAL" RECORD                 
087603*    -- RECORD, IN THE SAME WAY AS THE REQUEST HEADER.                    
087703                                                                          
087803     UNSTRING RECV-AREA (1:RECV-KVDLEN)                                   
087903       DELIMITED BY VBAR                                                  
087904       INTO                                                               
088003         ASSI-IDRTYP3IV     COUNT IN LIDRTYP3IV                           
088103         ASSI-IDPRC         COUNT IN LIDPRC                               
088203       OVERFLOW                                                           
088303**     -- TOO MANY FIELDS                                                 
088403         MOVE ERR-INV-MSG-STRUCTURE TO MED-IDMFSFEL                       
088503         CALL WMEDKONV USING MED-WMEDAREA                                 
088603         SET REQUEST-WRONG TO TRUE                                        
088703     END-UNSTRING                                                         
088803                                                                          
088903*--  ASSI-IDRTYP3IV                                                       
089003                                                                          
089103     IF REQUEST-OK                                                        
089203       IF ASSI-IDRTYP3IV NOT = 'AssignmentCriteria'                       
089303         MOVE ERR-WRONG-RCD-TYPE TO MED-IDMFSFEL                          
089403         CALL WMEDKONV USING MED-WMEDAREA                                 
089503         SET REQUEST-WRONG TO TRUE                                        
089603       END-IF                                                             
089703     END-IF                                                               
089803                                                                          
089903     IF REQUEST-OK                                                        
090003       IF LIDPRC > LENGTH OF ASSI-IDPRC                                   
090103**       --- PRODUCTION CHANNEL TO LONG                                   
090203         MOVE SPACE              TO TEMP-MESSAGE                          
090303         MOVE ERR-VALUE-TOO-LONG TO MED-IDMFSFEL                          
090403         CALL WMEDKONV USING MED-WMEDAREA                                 
090503         STRING 'PRODUKTIONSKANAL ' MED-TEMFSFEL                          
090603           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
090703         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
090803         SET REQUEST-WRONG TO TRUE                                        
090903       END-IF                                                             
091003     END-IF                                                               
091103                                                                          
091203     IF REQUEST-OK                                                        
091303       IF ASSI-IDPRCBAS NOT NUMERIC                                       
091403       OR ASSI-IDPRCBAS = ZEROES                                          
091503*        --- INVALID REQU-IDPRC                                           
091603         MOVE SPACE              TO TEMP-MESSAGE                          
091703         MOVE ERR-INVALID-VALUE TO MED-IDMFSFEL                           
091803         CALL WMEDKONV USING MED-WMEDAREA                                 
091903         STRING 'PRODUKTIONSKANAL ' MED-TEMFSFEL                          
092003           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
092103         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
092203         SET REQUEST-WRONG TO TRUE                                        
092303       ELSE                                                               
092403         MOVE ASSI-IDPRC     TO WS-IDPRC                                  
092503         MOVE WS-IDPRCBAS TO W-Q3BSEQ-MIN-IDPRCBAS                        
092603         MOVE WS-IDPRCBAS TO W-Q3BSEQ-MAX-IDPRCBAS                        
092703         IF WS-IDPRCVAR NOT = SPACE                                       
092803            MOVE WS-IDPRCVAR TO W-Q3BSEQ-MIN-IDPRCVAR                     
092903            MOVE WS-IDPRCVAR TO W-Q3BSEQ-MAX-IDPRCVAR                     
093003         END-IF                                                           
093103       END-IF                                                             
093203     END-IF                                                               
093303     .                                                                    
093403                                                                          
093503     EJECT                                                                
093603 BC-GET-LOCAL-DATE-AND-TIME SECTION.                                      
093703     MOVE 'BC-GET-LOCAL-DATE-AND-TIME'  TO WS-CURRENT-SECTION             
093803                                                                          
093903*    -- HÄMTA LOKALT DATUM OCH TID FÖR LAGRET (WIDDCxx)                   
094003     STRING 'WIDDC' REQU-IDDC ' '                                         
094103         DELIMITED BY SIZE INTO MSGI-IDUSER                               
094203     MOVE MSGI-IDUSER          TO MSGI-IDLTERM-USER                       
094303     ACCEPT WS-DATUM   FROM DATE                                          
094403     ACCEPT WS-KLOCKAN FROM TIME                                          
094503     MOVE WS-DATUM             TO MSGI-TILOKDAT                           
094603     MOVE WS-TIHHMMSS(1:4)     TO MSGI-TILOKTID                           
094703     MOVE '011'                TO MSGI-KDCALL                             
094803     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
094903     MOVE MSGI-TILOKDAT        TO WS-DATUM-LOK                            
095003*    -- SS FRÅN WS-TIHHMMSS, HHMM FRÅN TILOKTID                           
095103     MOVE WS-TIHHMMSS          TO WS-TIHHMMSS-LOK                         
095203     MOVE MSGI-TILOKTID        TO WS-TIHHMMSS-LOK(1:4)                    
095303     .                                                                    
095403                                                                          
095503     EJECT                                                                
095603 C-PROCESS-RESPONSE            SECTION.                                   
095703     MOVE 'C-PROCESS-RESPONSE  '  TO WS-CURRENT-SECTION                   
095803                                                                          
095903*    -- MOST DATA FOR THE RESPONSE WILL PROBABLY ALREADY HAVE             
096003*    -- BEEN SET IN PREVIOUS SECTIONS. OTHERWISE IT IS TIME TO            
096103*    -- DO IT NOW.                                                        
096203     MOVE FUNCTION CURRENT-DATE(1:16) TO RESP-TISTAMP3IV                  
096303     MOVE '0'                         TO RESP-TISTAMP3IV(17:1)            
096403*    -- ...                                                               
096503                                                                          
096603     PERFORM CA-PROCESS-RESPONSE-HEADER                                   
096703     PERFORM S04-SEND-MESSAGE                                             
096803     .                                                                    
096903     EJECT                                                                
097003 CA-PROCESS-RESPONSE-HEADER SECTION.                                      
097103     MOVE 'CA-PROCESS-RESPONSE-HEADER  '  TO WS-CURRENT-SECTION           
097203                                                                          
097303     MOVE SPACE TO SEND-AREA                                              
097403     MOVE 1     TO SEND-KVDLEN                                            
097503                                                                          
097603*    -- ALPHANUMERIC - REMOVE TRAILING SPACE                              
097703     MOVE ZERO TO LIDMSG3IV                                               
097803     INSPECT FUNCTION REVERSE(RESP-IDMSG3IV)                              
097903       TALLYING LIDMSG3IV FOR LEADING SPACE                               
098003     COMPUTE LIDMSG3IV = LENGTH OF RESP-IDMSG3IV - LIDMSG3IV              
098103     STRING RESP-IDMSG3IV (1:LIDMSG3IV)  VBAR                             
098203       DELIMITED BY SIZE                                                  
098303       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
098403                                                                          
098503     STRING 'ResponseAssignment' VBAR                                     
098603       DELIMITED BY SIZE                                                  
098703       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
098803                                                                          
098903     STRING RESP-TISTAMP3IV VBAR                                          
099003       DELIMITED BY SIZE                                                  
099103       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
099203                                                                          
099303     STRING RESP-IDDC VBAR                                                
099403       DELIMITED BY SIZE                                                  
099503       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
099603                                                                          
099703*    -- NUMERIC - REMOVE LEADING SPACE                                    
099803     MOVE 1 TO LIDANSTNR                                                  
099903     INSPECT RESP-IDANSTNR                                                
100003       TALLYING LIDANSTNR FOR LEADING SPACE                               
100103     STRING RESP-IDANSTNR (LIDANSTNR:) VBAR                               
100203       DELIMITED BY SIZE                                                  
100303       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
100403                                                                          
100503*    -- ALPHANUMERIC - REMOVE TRAILING SPACE                              
100603     MOVE ZERO TO LIDSNO3IV                                               
100703     INSPECT FUNCTION REVERSE(RESP-IDSNO3IV)                              
100803       TALLYING LIDSNO3IV FOR LEADING SPACE                               
100903     COMPUTE LIDSNO3IV = LENGTH OF RESP-IDSNO3IV - LIDSNO3IV              
101003     STRING RESP-IDSNO3IV (1:LIDSNO3IV)  VBAR                             
101103       DELIMITED BY SIZE                                                  
101203       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
101303                                                                          
101403*    -- NUMERIC - REMOVE LEADING SPACE                                    
101503     MOVE 1 TO LKDRESP3IV                                                 
101603     INSPECT RESP-KDRESP3IV                                               
101703       TALLYING LKDRESP3IV FOR LEADING SPACE                              
101803     STRING RESP-KDRESP3IV (LKDRESP3IV:) VBAR                             
101903       DELIMITED BY SIZE                                                  
102003       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
102103                                                                          
102203*    -- ALPHANUMERIC - REMOVE TRAILING SPACE                              
102303*    -- LAST FIELD! - NO TRAILING VERTICAL BAR                            
102403     MOVE ZERO TO LBERESP3IV                                              
102503     INSPECT FUNCTION REVERSE(RESP-BERESP3IV)                             
102603       TALLYING LBERESP3IV FOR LEADING SPACE                              
102703     COMPUTE LBERESP3IV = LENGTH OF RESP-BERESP3IV - LBERESP3IV           
102803     IF LBERESP3IV > ZERO                                                 
103103       STRING RESP-BERESP3IV (1:LBERESP3IV)                               
103203         DELIMITED BY SIZE                                                
103303         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
103304     END-IF                                                               
103305                                                                          
103306*    -- SEND-KVDLEN "POINTS" TO AFTER LAST CHARACTER IN MSG               
103307*    -- ADJUST TO CORRECT LENGTH                                          
103308     SUBTRACT 1 FROM SEND-KVDLEN                                          
103403     .                                                                    
103503     EJECT                                                                
103603 D-INIT-PLOCKSATSREG SECTION.                                             
103703     MOVE 'D-INIT-PLOCKSATSREG'  TO WS-CURRENT-SECTION                    
103803                                                                          
103903     MOVE WS-IDUSER              TO 4002-IDUSER                           
104003     MOVE WS-IDBORD              TO 4002-IDBORD                           
104103                                                                          
104203     MOVE REQU-IDMSG3IV          TO 4002-IDMSG3IV                         
104303     MOVE REQU-IDSNO3IV          TO 4002-IDSNO3IV                         
104403     MOVE RECV-ADDISPXTRA        TO 4002-ADDISPXTRA                       
104503                                                                          
104603     MOVE WS-CONSTANT-FLORDKNY   TO 4002-FLORDKNY                         
104703                                                                          
104803     MOVE 1                      TO 4002-KDSEGKEY                         
104903     MOVE NEJ                    TO 4002-FLORDSPL                         
105003     MOVE SPACE                  TO 4002-KDPRT-PLE                        
105103                                    4002-KDPRT-PU                         
105203                                    4002-KDSORT                           
105303                                                                          
105403     INITIALIZE                     4002-DEAL-PR-SUM                      
105503     MOVE ZERO                   TO 4002-IDLOPNR-PL                       
105603                                    4002-IXHEL                            
105703                                    4002-KVORDSPL                         
105803                                    4002-KVRADER                          
105903                                    4002-VKORDNTO                         
106003                                    4002-VLORDNTO                         
106103                                    4002-SUORDV                           
106203                                                                          
106303     MOVE 1 TO IX1                                                        
106403     PERFORM UNTIL IX1 > 99                                               
106503        MOVE LOW-VALUE TO 4002-ORDDEL (IX1)                               
106603        ADD 1 TO IX1                                                      
106703     END-PERFORM                                                          
106803     .                                                                    
106903     EJECT                                                                
107003 E-SKAPA-URVAL-PLK-LST SECTION.                                           
107103     MOVE 'E-SKAPA-URVAL-PLK-LST   ' TO WS-CURRENT-SECTION                
107203                                                                          
107303     MOVE 0   TO WS-KVORDER                                               
107403                 WS-KVRADER                                               
107503                                                                          
107603     PERFORM EB-LAES-FORSTA-ORDERDEL                                      
107703     IF ORDERDEL-SAKNAS                                                   
107803        MOVE SPACE            TO TEMP-MESSAGE                             
107903        MOVE ERR-ORDERDELAR-SAKNAS     TO MED-IDMFSFEL                    
108003        CALL WMEDKONV USING MED-WMEDAREA                                  
108103        SET REQUEST-WRONG TO TRUE                                         
108203     ELSE                                                                 
108303                                                                          
108403        PERFORM EA-KOLLA-ORDERDEL-OM-TA-MED                               
108503*       IF TA-MED-ORDERDEL                                                
108603*       -- FÖRSTA ORDERDELEN KOMMER ALLTID MED                            
108703          PERFORM EG-UPPLAGG-PLOCKSATSREG                                 
108803          PERFORM EH-UPPDAT-ORDERDEL                                      
108903*       END-IF                                                            
109003                                                                          
109103        PERFORM UNTIL ORDERDEL-Q3B1-SAKNAS                                
109203                   OR 4002-IXHEL > 99                                     
109303                   OR END-OF-DATA                                         
109403                   OR BRYTA                                               
109503                                                                          
109603           IF VARIANT-STJARNORDER                                         
109703             PERFORM EC-LAES-NASTA-ORDDEL-STJARNORD                       
109803           ELSE                                                           
109903             PERFORM EF-LAES-NASTA-ORDERDEL                               
110003           END-IF                                                         
110103                                                                          
110203           IF ORDERDEL-FINNS                                              
110303             PERFORM EA-KOLLA-ORDERDEL-OM-TA-MED                          
110403             IF  TA-MED-ORDERDEL                                          
110503                PERFORM EG-UPPLAGG-PLOCKSATSREG                           
110603                PERFORM EH-UPPDAT-ORDERDEL                                
110703             END-IF                                                       
110803           END-IF                                                         
110903                                                                          
111003        END-PERFORM                                                       
111103                                                                          
111203        PERFORM EI-SKAPA-PLOCKSATS                                        
111303     END-IF                                                               
111403     .                                                                    
111503     EJECT                                                                
111603                                                                          
111703 EA-KOLLA-ORDERDEL-OM-TA-MED SECTION.                                     
111803     MOVE 'EA-KOLLA-ORDERDEL-OM-TA-MED ' TO WS-CURRENT-SECTION            
111903                                                                          
112003     EVALUATE TRUE                                                        
112103       WHEN VARIANT-PLOCKGRANS                                            
112203         PERFORM EAA-KOLLA-PLOCKGRANS                                     
112303         MOVE JA TO TA-MED-ORDERDEL-SW                                    
112403                                                                          
112503       WHEN VARIANT-STJARNORDER                                           
112603         IF  ORQA-ODEL-IDDISTR  = SPAR-IDDISTR                            
112703         AND ORQA-ODEL-IDKUNDNR = SPAR-IDKUNDNR                           
112803            MOVE JA  TO TA-MED-ORDERDEL-SW                                
112903         ELSE                                                             
113003            MOVE NEJ TO TA-MED-ORDERDEL-SW                                
113103         END-IF                                                           
113203                                                                          
113303       WHEN VARIANT-UTAN-STJARNORDER                                      
113403*ORDERKO-TABELL                                                           
113503         SET ORDERKO-IX      TO 1                                         
113603                                                                          
113703         SEARCH ORDERKO-LIST                                              
113803            AT END                                                        
113903              CONTINUE                                                    
114003         WHEN ORQA-ODEL-IDDISTR  = ORDERKO-IDDISTR (ORDERKO-IX)           
114103          AND ORQA-ODEL-IDKUNDNR = ORDERKO-IDKUNDNR(ORDERKO-IX)           
114203                                                                          
114303              IF ORDERKO-FLER(ORDERKO-IX) = '*'                           
114403                MOVE NEJ TO TA-MED-ORDERDEL-SW                            
114503              ELSE                                                        
114603                MOVE JA TO TA-MED-ORDERDEL-SW                             
114703                PERFORM EAA-KOLLA-PLOCKGRANS                              
114803                IF PLOCKGRANS                                             
114903                  MOVE JA TO BRYTA-SW                                     
115003                END-IF                                                    
115103              END-IF                                                      
115203         END-SEARCH                                                       
115303                                                                          
115403     END-EVALUATE                                                         
115503     .                                                                    
115603     EJECT                                                                
115703 EAA-KOLLA-PLOCKGRANS SECTION.                                            
115803     MOVE 'EAA-KOLLA-PLOCKGRANS     ' TO WS-CURRENT-SECTION               
115903                                                                          
116003     ADD 1                   TO WS-KVORDER                                
116103     ADD ORQA-ODEL-KVRADER   TO WS-KVRADER                                
116203     ADD ORQA-ODEL-VKORDNTO  TO WS-VKORDNTO                               
116303     ADD ORQA-ODEL-VLORDNTO  TO WS-VLORDNTO                               
116403                                                                          
116503     IF WS-KVORDER    = WS-PRC-KVORDER                                    
116603     OR WS-KVRADER   >= WS-PRC-KVRADER                                    
116703     OR WS-VLORDNTO  >= WS-PRC-VLORDNTO                                   
116803     OR WS-VKORDNTO  >= WS-PRC-VKORDNTO                                   
116903        MOVE JA TO PLOCKGRANS-SW                                          
117003     END-IF                                                               
117103                                                                          
117203     IF PLOCKGRANS                                                        
117303       IF WS-PRC-KVORDER = 1                                              
117403          IF WS-KVRADER    > WS-PRC-KVRADER                               
117503          OR WS-VLORDNTO   > WS-PRC-VLORDNTO                              
117603          OR WS-VKORDNTO   > WS-PRC-VKORDNTO                              
117703             MOVE JA TO SPLIT-SW                                          
117803          END-IF                                                          
117903       END-IF                                                             
118003                                                                          
118103       MOVE JA TO BRYTA-SW                                                
118203     END-IF                                                               
118303                                                                          
118403     IF SPLIT                                                             
118503        PERFORM EAAA-BERAKNA-ANTAL-SPLITSATSER                            
118603     END-IF                                                               
118703     .                                                                    
118803     EJECT                                                                
118903 EAAA-BERAKNA-ANTAL-SPLITSATSER SECTION.                                  
119003     MOVE 'EAAA-BERAKNA-ANTAL-SPLIT ' TO WS-CURRENT-SECTION               
119103                                                                          
119203     MOVE 0 TO WS-PROC-RAD                                                
119303               WS-PROC-VIKT                                               
119403               WS-PROC-VOLYM                                              
119503                                                                          
119603     IF WS-KVRADER > WS-PRC-KVRADER                                       
119703        COMPUTE WS-PROC-RAD =                                             
119803                WS-KVRADER * 100 / WS-PRC-KVRADER                         
119903                ON SIZE ERROR MOVE 0 TO WS-PROC-RAD                       
120003        END-COMPUTE                                                       
120103     END-IF                                                               
120203                                                                          
120303     IF WS-VKORDNTO > WS-PRC-VKORDNTO                                     
120403        COMPUTE WS-PROC-VIKT =                                            
120503                WS-VKORDNTO * 100 / WS-PRC-VKORDNTO                       
120603                ON SIZE ERROR MOVE 0 TO WS-PROC-VIKT                      
120703        END-COMPUTE                                                       
120803     END-IF                                                               
120903                                                                          
121003     IF WS-VLORDNTO > WS-PRC-VLORDNTO                                     
121103        COMPUTE WS-PROC-VOLYM =                                           
121203                WS-VLORDNTO * 100 / WS-PRC-VLORDNTO                       
121303                ON SIZE ERROR MOVE 0 TO WS-PROC-VOLYM                     
121403        END-COMPUTE                                                       
121503     END-IF                                                               
121603                                                                          
121703     IF WS-PROC-RAD NOT < WS-PROC-VIKT                                    
121803        AND                                                               
121903        WS-PROC-RAD NOT < WS-PROC-VOLYM                                   
122003        MOVE WS-KVRADER     TO WS-SPLITGRANS                              
122103        MOVE WS-PRC-KVRADER TO WS-PRC-SPLITGRANS                          
122203        MOVE 'RA'           TO WS-KDSORT                                  
122303     ELSE                                                                 
122403        IF WS-PROC-VIKT NOT < WS-PROC-RAD                                 
122503           AND                                                            
122603           WS-PROC-VIKT NOT < WS-PROC-VOLYM                               
122703           MOVE WS-VKORDNTO     TO WS-SPLITGRANS                          
122803           MOVE WS-PRC-VKORDNTO TO WS-PRC-SPLITGRANS                      
122903           MOVE 'KG'            TO WS-KDSORT                              
123003        ELSE                                                              
123103           IF WS-PROC-VOLYM NOT < WS-PROC-RAD                             
123203              AND                                                         
123303              WS-PROC-VOLYM NOT < WS-PROC-VIKT                            
123403              MOVE WS-VLORDNTO     TO WS-SPLITGRANS                       
123503              MOVE WS-PRC-VLORDNTO TO WS-PRC-SPLITGRANS                   
123603              MOVE 'M3'            TO WS-KDSORT                           
123703           END-IF                                                         
123803        END-IF                                                            
123903     END-IF                                                               
124003                                                                          
124103     COMPUTE WS-ANTSPLIT = WS-SPLITGRANS / WS-PRC-SPLITGRANS              
124203             ON SIZE ERROR MOVE ZERO TO WS-ANTSPLIT                       
124303     END-COMPUTE                                                          
124403                                                                          
124503     IF WS-ANTSPLIT (9:2) > ZERO                                          
124603        MOVE ZERO              TO WS-ANTSPLIT (9:2)                       
124703        COMPUTE WS-SPLITREST = WS-SPLITGRANS -                            
124803                              (WS-ANTSPLIT * WS-PRC-SPLITGRANS)           
124903        END-COMPUTE                                                       
125003        COMPUTE WS-RESPLIT =  WS-SPLITREST / WS-PRC-SPLITGRANS            
125103                ON SIZE ERROR MOVE ZERO TO WS-RESPLIT                     
125203        END-COMPUTE                                                       
125303        IF WS-RESPLIT >= WS-PRC-RESPLIT                                   
125403           ADD 1 TO WS-ANTSPLIT                                           
125503        END-IF                                                            
125603     END-IF                                                               
125703                                                                          
125803     IF WS-ANTSPLIT > 1                                                   
125903        MOVE WS-PRC-SPLITGRANS TO 4002-KVORDSPL                           
126003        MOVE WS-KDSORT         TO 4002-KDSORT                             
126103        MOVE JA                TO 4002-FLORDSPL                           
126203     ELSE                                                                 
126303        MOVE NEJ               TO SPLIT-SW                                
126403     END-IF                                                               
126503     .                                                                    
126603     EJECT                                                                
126703 EB-LAES-FORSTA-ORDERDEL SECTION.                                         
126803     MOVE 'EB-LAES-FORSTA-ORDERDEL ' TO WS-CURRENT-SECTION                
126903                                                                          
127003     MOVE REQU-IDDC     TO W-Q3B1-MIN-IDDC                                
127103     MOVE REQU-IDDC     TO W-Q3B1-MAX-IDDC                                
127203     MOVE WS-IDPRCBAS   TO W-Q3B1-MIN-IDPRCBAS                            
127303     MOVE WS-IDPRCBAS   TO W-Q3B1-MAX-IDPRCBAS                            
127403     MOVE WS-IDPRCVAR   TO W-Q3B1-IDPRCVAR                                
127503                                                                          
127603     MOVE NEJ           TO TA-MED-ORDERDEL-SW                             
127703     MOVE NEJ           TO WS-ORQI-OHUV-FLKLAR                            
127803                                                                          
127903     PERFORM IMS-GHU-WDQ3B1                                               
128003     IF SEGMENT-SAKNAS OR END-OF-DATA                                     
128103       MOVE JA          TO ORDERDEL-SW                                    
128203     END-IF                                                               
128303     PERFORM UNTIL ORDERDEL-Q3B1-SAKNAS                                   
128403                OR END-OF-DATA                                            
128503                OR WS-ORQI-OHUV-FLKLAR = JA                               
128603                                                                          
128703       MOVE Q3B1-SEQB-IDORDER        TO W-IDORDER                         
128803       MOVE Q3B1-SEQB-IDORDER        TO W-Q301KY-IDORDER                  
128903       MOVE Q3B1-SEQB-IDDC           TO W-Q301KY-IDDC                     
129003       MOVE Q3B1-SEQB-IDPRODNR       TO W-Q301KY-IDPRODNR                 
129103       MOVE Q3B1-SEQB-IDPLKLST       TO W-Q301KY-IDPLKLST                 
129203       PERFORM IMS-GHU-WDQ301                                             
129303                                                                          
129403       IF ORQA-ODEL-IDUSER = SPACE                                        
129503       OR ORQA-ODEL-IDUSER = WS-IDUSER                                    
129603                                                                          
129703         MOVE ORQA-ODEL-IDORDER     TO W-IDORDER                          
129803         MOVE ORQA-ODEL-IDDC        TO W-IDDC                             
129903         PERFORM IMS-GHU-ORQI-WLORQI01-WLORQI12                           
130003                                                                          
130103*FIX för att klara "trasig orderdata" dvs orddel utan ordhuvud            
130203*        IF SEGMENT-FINNS                                                 
130303           MOVE ORQI-OHUV-FLKLAR TO WS-ORQI-OHUV-FLKLAR                   
130403*        ELSE                                                             
130503*          SEND MAIL?                                                     
130603*        END-IF                                                           
130703       END-IF                                                             
130803       PERFORM IMS-GHN-WDQ3B1                                             
130903                                                                          
131003     END-PERFORM                                                          
131103                                                                          
131203     MOVE ZERO               TO WS-KVORDER                                
131303                                WS-KVRADER                                
131403                                WS-VKORDNTO                               
131503                                WS-VLORDNTO                               
131603     MOVE  NEJ               TO WS-ORQI-OHUV-FLKLAR                       
131703     .                                                                    
131803     EJECT                                                                
131903 EC-LAES-NASTA-ORDDEL-STJARNORD   SECTION.                                
132003     MOVE 'EC-LAES-NASTA-ORDERDEL-STJ ' TO WS-CURRENT-SECTION             
132103                                                                          
132203     PERFORM UNTIL ORDERDEL-Q3B1-SAKNAS                                   
132303                OR END-OF-DATA                                            
132403                OR WS-ORQI-OHUV-FLKLAR = JA                               
132503                                                                          
132603       MOVE Q3B1-SEQB-IDORDER        TO W-IDORDER                         
132703       MOVE Q3B1-SEQB-IDORDER        TO W-Q301KY-IDORDER                  
132803       MOVE Q3B1-SEQB-IDDC           TO W-Q301KY-IDDC                     
132903       MOVE Q3B1-SEQB-IDPRODNR       TO W-Q301KY-IDPRODNR                 
133003       MOVE Q3B1-SEQB-IDPLKLST       TO W-Q301KY-IDPLKLST                 
133103       PERFORM IMS-GHU-WDQ301                                             
133203                                                                          
133303       IF ORQA-ODEL-IDUSER = SPACE                                        
133403       OR ORQA-ODEL-IDUSER = WS-IDUSER                                    
133503                                                                          
133603         MOVE ORQA-ODEL-IDORDER     TO W-IDORDER                          
133703         MOVE ORQA-ODEL-IDDC        TO W-IDDC                             
133803         PERFORM IMS-GHU-ORQI-WLORQI01-WLORQI12                           
133903                                                                          
134003*FIX för att klara "trasig orderdata" dvs orddel utan ordhuvud            
134103*        IF SEGMENT-FINNS                                                 
134203           MOVE ORQI-OHUV-FLKLAR    TO WS-ORQI-OHUV-FLKLAR                
134303           MOVE NEJ                 TO ORDERDEL-SW                        
134403*        ELSE                                                             
134503*          SEND MAIL?                                                     
134603*        END-IF                                                           
134703       END-IF                                                             
134803                                                                          
134903       PERFORM IMS-GHN-WDQ3B1                                             
135003     END-PERFORM                                                          
135103     MOVE  NEJ               TO WS-ORQI-OHUV-FLKLAR                       
135203     .                                                                    
135303     EJECT                                                                
135403 EF-LAES-NASTA-ORDERDEL SECTION.                                          
135503     MOVE ' F-LAES-NASTA-ORDERDEL  ' TO WS-CURRENT-SECTION                
135603                                                                          
135703     PERFORM UNTIL ORDERDEL-Q3B1-SAKNAS                                   
135803                OR END-OF-DATA                                            
135903                OR WS-ORQI-OHUV-FLKLAR = JA                               
136003                                                                          
136103       MOVE Q3B1-SEQB-IDORDER        TO W-IDORDER                         
136203       MOVE Q3B1-SEQB-IDORDER        TO W-Q301KY-IDORDER                  
136303       MOVE Q3B1-SEQB-IDDC           TO W-Q301KY-IDDC                     
136403       MOVE Q3B1-SEQB-IDPRODNR       TO W-Q301KY-IDPRODNR                 
136503       MOVE Q3B1-SEQB-IDPLKLST       TO W-Q301KY-IDPLKLST                 
136603       PERFORM IMS-GHU-WDQ301                                             
136703                                                                          
136803       IF ORQA-ODEL-IDUSER = SPACE                                        
136903       OR ORQA-ODEL-IDUSER = WS-IDUSER                                    
137003         IF (ORQA-ODEL-KVRADER  <= WS-PRC-KVPLSRAD                        
137103         AND ORQA-ODEL-VKORDNTO <= WS-PRC-VKPLSNTO                        
137203         AND ORQA-ODEL-VLORDNTO <= WS-PRC-VLPLSNTO)                       
137303                                                                          
137403           MOVE ORQA-ODEL-IDORDER   TO W-IDORDER                          
137503           MOVE ORQA-ODEL-IDDC      TO W-IDDC                             
137603           PERFORM IMS-GHU-ORQI-WLORQI01-WLORQI12                         
137703*FIX för att klara "trasig orderdata" dvs orddel utan ordhuvud            
137803*          IF SEGMENT-FINNS                                               
137903             MOVE ORQI-OHUV-FLKLAR TO WS-ORQI-OHUV-FLKLAR                 
138003*          ELSE                                                           
138103*            SEND MAIL?                                                   
138203*          END-IF                                                         
138303*        ELSE                                                             
138403         END-IF                                                           
138500       END-IF                                                             
138600                                                                          
138700       PERFORM IMS-GHN-WDQ3B1                                             
138800     END-PERFORM                                                          
138900     MOVE  NEJ               TO WS-ORQI-OHUV-FLKLAR                       
139000                                                                          
139100     .                                                                    
139200     EJECT                                                                
139300 EG-UPPLAGG-PLOCKSATSREG SECTION.                                         
139400     MOVE 'EG-UPPLAGG-PLOCKSATSREG ' TO WS-CURRENT-SECTION                
139500                                                                          
139600     ADD 1                   TO 4002-IXHEL                                
139700     MOVE ORQA-ODEL-IDORDER  TO 4002-IDORDER  (4002-IXHEL)                
139800     MOVE ORQA-ODEL-IDDC     TO 4002-IDDC     (4002-IXHEL)                
139900     MOVE ORQA-ODEL-IDPRODNR TO 4002-IDPRODNR (4002-IXHEL)                
140000     MOVE ORQA-ODEL-IDPLKLST TO 4002-IDPLKLST (4002-IXHEL)                
140100                                                                          
140200     IF SPLIT                                                             
140300        PERFORM EGA-HAMTA-NASTA-PLOCKLISTNR                               
140400     END-IF                                                               
140500     .                                                                    
140600     EJECT                                                                
140700 EGA-HAMTA-NASTA-PLOCKLISTNR SECTION.                                     
140800     MOVE 'EGA-HAMTA-NASTA-PLOCKLISTNR'  TO WS-CURRENT-SECTION            
140900                                                                          
141000     MOVE 4002-ORDDEL (4002-IXHEL) TO 4002-ORDDEL (99)                    
141100     COMPUTE ORQI-ARB-IDPLKLST-SISTA                                      
141200                      = ORQI-ARB-IDPLKLST-SISTA + 1                       
141300     MOVE ORQI-ARB-IDPLKLST-SISTA TO 4002-IDPLKLST (99)                   
141400                                                                          
141500     PERFORM IMS-REPL-ORQI-WLORQI12                                       
141600     .                                                                    
141700     EJECT                                                                
141800 EH-UPPDAT-ORDERDEL        SECTION.                                       
141900     MOVE 'EH-UPPDAT-ORDERDEL     '  TO WS-CURRENT-SECTION                
142000                                                                          
142100     MOVE WS-IDUSER       TO ORQA-ODEL-IDUSER                             
142200     MOVE WS-IDBORD       TO ORQA-ODEL-IDBORD                             
142300     MOVE WS-DATUM-LOK    TO ORQA-ODEL-DAUTSKR                            
142400     IF WS-DATUM-LOK NOT = ZERO                                           
142500       IF WS-DATUM-LOK < 500000                                           
142600         MOVE 20          TO ORQA-ODEL-DAUTSKR (1:2)                      
142700       ELSE                                                               
142800         IF WS-DATUM-LOK < 999999                                         
142900           MOVE 19        TO ORQA-ODEL-DAUTSKR (1:2)                      
143000         ELSE                                                             
143100           MOVE 99999999  TO ORQA-ODEL-DAUTSKR                            
143200         END-IF                                                           
143300       END-IF                                                             
143400     END-IF                                                               
143500     MOVE WS-TIHHMMSS-LOK TO ORQA-ODEL-TIUTSTID                           
143600     MOVE 'U'             TO ORQA-ODEL-KDODELSTA                          
143700     PERFORM IMS-REPL-ORQA-WLORQA01                                       
143800     .                                                                    
143900     EJECT                                                                
144000 EI-SKAPA-PLOCKSATS SECTION.                                              
144100     MOVE 'EI-SKAPA-PLOCKSATS     '  TO WS-CURRENT-SECTION                
144200                                                                          
144300     PERFORM EIB-SKAPA-PLOCKSATS                                          
144400*P-TO-P-SW1-VÄRDEN INITIERAS I EIB                                        
144503                                                                          
144603                                                                          
144703     PERFORM IMS-ISRT-MSG-ALT1                                            
144803*037 PLOCKSATS KÖAD FÖR UTSKRIFT                                          
144903     .                                                                    
145003     EJECT                                                                
145103 EIB-SKAPA-PLOCKSATS SECTION.                                             
145203     MOVE 'EIB-SKAPA-PLOCKSATS    '  TO WS-CURRENT-SECTION                
145303                                                                          
145403     IF 4002-ORDDEL (1) NOT = LOW-VALUE                                   
145503        PERFORM EIBA-HAMTA-PLOCKSATSNR                                    
145603        MOVE 4002-IDPRODNR (1) TO W-4001-IDPRODNR                         
145703                                  PTOP1-MID-IDPRODNR                      
145803        MOVE 4002-IDPLKLST (1) TO W-4001-IDPLKLST                         
145903                                  PTOP1-MID-IDPLKLST                      
146003        MOVE W-4001-IDHTYP-X   TO WL400101                                
146103        PERFORM IMS-ISRT-4001-WL400101                                    
146203        MOVE ZERO              TO 4002-IXHEL                              
146303        PERFORM IMS-ISRT-4001-WL400111                                    
146403     END-IF                                                               
146503     .                                                                    
146603     EJECT                                                                
146703 EIBA-HAMTA-PLOCKSATSNR SECTION.                                          
146803     MOVE 'EIBA-HAMTA-PLOCKSATSNR '  TO WS-CURRENT-SECTION                
146903                                                                          
147003     MOVE REQU-IDDC  TO W-4461-IDDC                                       
147103     PERFORM IMS-GHU-XXKO-WLXXKO11                                        
147203     IF SEGMENT-FINNS                                                     
147303        IF XXKO-4462-IDLOPNR-PL = 999                                     
147403           MOVE WS-DATUM-LOK      TO XXKO-4462-TIDATUM                    
147503           MOVE 1                 TO XXKO-4462-IDLOPNR-PL                 
147603        ELSE                                                              
147703           ADD  1                 TO XXKO-4462-IDLOPNR-PL                 
147803        END-IF                                                            
147903        MOVE XXKO-4462-IDLOPNR-PL TO 4002-IDLOPNR-PL                      
148003        PERFORM IMS-REPL-XXKO-WLXXKO11                                    
148103     ELSE                                                                 
148203        MOVE 1                    TO 4002-IDLOPNR-PL                      
148303     END-IF                                                               
148403     .                                                                    
148503     EJECT                                                                
148603 F-LAES-PRC-KANAL SECTION.                                                
148703     MOVE 'F-LAES-PRC-KANAL       '  TO WS-CURRENT-SECTION                
148803                                                                          
148903     MOVE REQU-IDDC       TO W-4447-IDDC                                  
149003     MOVE WS-IDPRC        TO W-4448-IDPRC                                 
149103                                                                          
149203     PERFORM IMS-GU-XXKH-WLXXKH11                                         
149303                                                                          
149403     IF SEGMENT-FINNS                                                     
149503        MOVE XXKH-4448-KVORDER  TO WS-PRC-KVORDER                         
149603        MOVE XXKH-4448-KVRADER  TO WS-PRC-KVRADER                         
149703        MOVE XXKH-4448-VKORDNTO TO WS-PRC-VKORDNTO                        
149803        MOVE XXKH-4448-VLORDNTO TO WS-PRC-VLORDNTO                        
149903        MOVE XXKH-4448-KVPLSRAD TO WS-PRC-KVPLSRAD                        
150003        MOVE XXKH-4448-VKPLSNTO TO WS-PRC-VKPLSNTO                        
150103        MOVE XXKH-4448-VLPLSNTO TO WS-PRC-VLPLSNTO                        
150203        MOVE XXKH-4448-RESPLIT  TO WS-PRC-RESPLIT                         
150303        MOVE XXKH-4448-KDPRCGRP TO W-4462-KDPRCGRP-X                      
150403*STJÄRNORDER                                                              
150503*       MOVE XXKH-4448-FLSTJORD TO WS-PRC-FLSTJORD                        
150603     END-IF                                                               
150703     .                                                                    
150803     EJECT                                                                
150903 G-KOLLA-FORSTA-ORDERDEL    SECTION.                                      
151003     MOVE 'G-KOLLA-FORSTA-ORDERDEL'  TO WS-CURRENT-SECTION                
151103                                                                          
151203     MOVE NEJ           TO WS-MINST-EN-ORDERDEL-HITTAD                    
151303     MOVE JA            TO DUBLETT-PA-FORSTA-OD-SAKNAS-SW                 
151403     MOVE REQU-IDDC     TO W-Q3B1-IDDC                                    
151503     MOVE REQU-IDDC     TO W-Q3B1-MIN-IDDC                                
151603     MOVE REQU-IDDC     TO W-Q3B1-MAX-IDDC                                
151703                                                                          
151803     MOVE WS-IDPRCBAS   TO W-Q3B1-IDPRCBAS                                
151903     MOVE WS-IDPRCBAS   TO W-Q3B1-MIN-IDPRCBAS                            
152003     MOVE WS-IDPRCBAS   TO W-Q3B1-MAX-IDPRCBAS                            
152103     MOVE WS-IDPRCVAR   TO W-Q3B1-IDPRCVAR                                
152203     MOVE WS-IDPRCVAR   TO W-Q3B1-MIN-IDPRCVAR                            
152303     MOVE WS-IDPRCVAR   TO W-Q3B1-MAX-IDPRCVAR                            
152403                                                                          
152503     PERFORM IMS-GN-WDQ3B1                                                
152603     PERFORM UNTIL SEGMENT-SAKNAS                                         
152703                OR END-OF-DATA                                            
152803                OR ORQI-OHUV-FLKLAR = JA                                  
152903                                                                          
153003       MOVE Q3B1-SEQB-IDORDER        TO W-IDORDER                         
153103                                                                          
153203       PERFORM IMS-GHU-ORQI-WLORQI01                                      
153303                                                                          
153403*FIX för att klara "trasig orderdata" dvs orddel utan ordhuvud            
153503*      IF SEGMENT-FINNS                                                   
153603         MOVE ORQI-OHUV-FLKLAR TO WS-ORQI-OHUV-FLKLAR                     
153703         IF ORQI-OHUV-FLKLAR = JA                                         
153803           MOVE Q3B1-SEQB-IDDISTR    TO SPAR-IDDISTR                      
153903           MOVE Q3B1-SEQB-IDKUNDNR   TO SPAR-IDKUNDNR                     
154003           MOVE JA                  TO WS-MINST-EN-ORDERDEL-HITTAD        
154103                                                                          
154203           PERFORM IMS-GN-WDQ3B1-DISTR-KUND                               
154303           IF SEGMENT-SAKNAS                                              
154403             MOVE NEJ  TO DUBLETT-PA-FORSTA-OD-SAKNAS-SW                  
154503           END-IF                                                         
154603          ELSE                                                            
154703            PERFORM IMS-GN-WDQ3B1                                         
154803          END-IF                                                          
154903*      ELSE                                                               
155003*        SEND MAIL?                                                       
155103*        PERFORM IMS-GN-WDQ3B1                                            
155203*      END-IF                                                             
155303     END-PERFORM                                                          
155403     .                                                                    
155503     EJECT                                                                
155603*                                                                         
155703 I-KOLLA-FORSTA-ORDERDEL    SECTION.                                      
155803     MOVE 'I-KOLLA-FORSTA-ORDERDEL'  TO WS-CURRENT-SECTION                
155903                                                                          
156003     MOVE NEJ           TO WS-MINST-EN-ORDERDEL-HITTAD                    
156103     MOVE REQU-IDDC     TO W-Q3B1-IDDC                                    
156203     MOVE REQU-IDDC     TO W-Q3B1-MIN-IDDC                                
156303     MOVE REQU-IDDC     TO W-Q3B1-MAX-IDDC                                
156403                                                                          
156503     MOVE WS-IDPRCBAS   TO W-Q3B1-IDPRCBAS                                
156603     MOVE WS-IDPRCBAS   TO W-Q3B1-MIN-IDPRCBAS                            
156703     MOVE WS-IDPRCBAS   TO W-Q3B1-MAX-IDPRCBAS                            
156803     MOVE WS-IDPRCVAR   TO W-Q3B1-IDPRCVAR                                
156903     MOVE WS-IDPRCVAR   TO W-Q3B1-MIN-IDPRCVAR                            
157003     MOVE WS-IDPRCVAR   TO W-Q3B1-MAX-IDPRCVAR                            
157103                                                                          
157203     PERFORM IMS-GU-WDQ3B1                                                
157303     PERFORM UNTIL SEGMENT-SAKNAS                                         
157403                OR END-OF-DATA                                            
157503                OR ORQI-OHUV-FLKLAR = JA                                  
157603                                                                          
157703       MOVE Q3B1-SEQB-IDORDER        TO W-IDORDER                         
157803       PERFORM IMS-GHU-ORQI-WLORQI01                                      
157903                                                                          
158003*FIX för att klara "trasig orderdata" dvs orddel utan ordhuvud            
158103*      IF SEGMENT-FINNS                                                   
158203         MOVE  ORQI-OHUV-FLKLAR         TO WS-ORQI-OHUV-FLKLAR            
158303         IF ORQI-OHUV-FLKLAR = JA                                         
158403           MOVE Q3B1-SEQB-IDDISTR       TO SPAR-IDDISTR                   
158503           MOVE Q3B1-SEQB-IDKUNDNR      TO SPAR-IDKUNDNR                  
158603           MOVE JA               TO WS-MINST-EN-ORDERDEL-HITTAD           
158703         ELSE                                                             
158803           PERFORM IMS-GN-WDQ3B1                                          
158903         END-IF                                                           
159003*      ELSE                                                               
160003*        SEND MAIL?                                                       
160103*        PERFORM IMS-GN-WDQ3B1                                            
160203*      END-IF                                                             
160303     END-PERFORM                                                          
160403     .                                                                    
160503     EJECT                                                                
160603*                                                                         
160703 H-SKAPA-ORDERKO-TABELL      SECTION.                                     
160803     MOVE 'H-SKAPA-ORDERKO-TABELL '  TO WS-CURRENT-SECTION                
160903                                                                          
161003*ORDERKO-TABELL                                                           
161103     MOVE ZERO                   TO WS-ANTAL-ORDERKO-RADER                
161203     MOVE JA                     TO FIRST-TIME-SW                         
161303                                                                          
161403     MOVE WS-IDPRCBAS            TO W-Q3B1-IDPRCBAS                       
161503     MOVE WS-IDPRCVAR            TO W-Q3B1-IDPRCVAR                       
161603     MOVE REQU-IDDC              TO W-Q3B1-MIN-IDDC                       
161703     MOVE REQU-IDDC              TO W-Q3B1-MAX-IDDC                       
161803                                                                          
161903     MOVE WS-IDPRCBAS            TO W-Q3B1-MIN-IDPRCBAS                   
162003     MOVE WS-IDPRCBAS            TO W-Q3B1-MAX-IDPRCBAS                   
162103     MOVE WS-IDPRCVAR            TO W-Q3B1-MIN-IDPRCVAR                   
162203     MOVE WS-IDPRCVAR            TO W-Q3B1-MAX-IDPRCVAR                   
162303                                                                          
162403     PERFORM IMS-GU-WDQ3B1                                                
162503     SET ORDERKO-IX      TO 1                                             
162603     MOVE 1              TO WS-ORDERKO-IX                                 
162703                                                                          
162803     PERFORM UNTIL ORDERKO-IX > ORDERKO-MAX                               
162903                OR SEGMENT-SAKNAS                                         
163003                OR END-OF-DATA                                            
163103                                                                          
163203       MOVE Q3B1-SEQB-IDORDER         TO W-IDORDER                        
163303                                                                          
163403       PERFORM IMS-GHU-ORQI-WLORQI01                                      
163503                                                                          
163603*FIX för att klara "trasig orderdata" dvs orddel utan ordhuvud            
163703*      IF SEGMENT-FINNS                                                   
163803         IF ORQI-OHUV-FLKLAR = JA                                         
163903           MOVE Q3B1-SEQB-IDDISTR  TO ORDERKO-IDDISTR (ORDERKO-IX)        
164003           MOVE Q3B1-SEQB-IDKUNDNR TO ORDERKO-IDKUNDNR(ORDERKO-IX)        
164103           SET ORDERKO-IX UP BY 1                                         
164203           ADD 1            TO WS-ORDERKO-IX                              
164303         END-IF                                                           
164403*      ELSE                                                               
164503*        CONTINUE                                                         
164603*        SEND MAIL?                                                       
164703*      END-IF                                                             
164803                                                                          
164903       PERFORM IMS-GN-WDQ3B1                                              
165003     END-PERFORM                                                          
165103                                                                          
165203     COMPUTE WS-ORDERKO-IX = WS-ORDERKO-IX - 1                            
165303     END-COMPUTE                                                          
165403     IF WS-ORDERKO-IX > +2000                                             
165503       MOVE ORDERKO-MAX          TO WS-ANTAL-ORDERKO-RADER                
165603     ELSE                                                                 
165703       MOVE WS-ORDERKO-IX        TO WS-ANTAL-ORDERKO-RADER                
165803     END-IF                                                               
165903                                                                          
166003     IF ORDERKO-IX > 1                                                    
166103       PERFORM HB-KOLLA-FLER-ORDER-PA-KUND                                
166203     END-IF                                                               
166303     .                                                                    
166403     EJECT                                                                
166503 HB-KOLLA-FLER-ORDER-PA-KUND          SECTION.                            
166603     MOVE 'HAB-KOLLA-FLER-ORDER-PA-KUND  '  TO WS-CURRENT-SECTION         
166703                                                                          
166803*  KOLLA PÅ SAMMA SIDA                                                    
166903     MOVE 1 TO IX1                                                        
167003     PERFORM UNTIL IX1 > WS-ANTAL-ORDERKO-RADER                           
167103                                                                          
167203         MOVE 'HB-KOLLA-FLER-POS1     '  TO WS-CURRENT-SECTION            
167303        MOVE 1 TO EXPAND-IX                                               
167403        PERFORM UNTIL EXPAND-IDDISTR(EXPAND-IX) = SPACE                   
167503        OR (EXPAND-IDDISTR(EXPAND-IX)  = ORDERKO-IDDISTR(IX1)             
167603        AND EXPAND-IDKUNDNR(EXPAND-IX) = ORDERKO-IDKUNDNR(IX1))           
167703                                                                          
167803           ADD +1 TO EXPAND-IX                                            
167903         MOVE 'HB-KOLLA-FLER-POS2     '  TO WS-CURRENT-SECTION            
168003        END-PERFORM                                                       
168103                                                                          
168203        IF  ORDERKO-IDDISTR(IX1)  = EXPAND-IDDISTR(EXPAND-IX)             
168303        AND ORDERKO-IDKUNDNR(IX1) = EXPAND-IDKUNDNR(EXPAND-IX)            
168403                                                                          
168503          MOVE '*' TO EXPAND-FLER(EXPAND-IX)                              
168603         MOVE 'HB-KOLLA-FLER-POS3     '  TO WS-CURRENT-SECTION            
168703        ELSE                                                              
168803          MOVE ORDERKO-IDDISTR(IX1)  TO EXPAND-IDDISTR(EXPAND-IX)         
168903          MOVE ORDERKO-IDKUNDNR(IX1) TO EXPAND-IDKUNDNR(EXPAND-IX)        
169003         MOVE 'HB-KOLLA-FLER-POS4     '  TO WS-CURRENT-SECTION            
169103        END-IF                                                            
169203        ADD +1 TO IX1                                                     
169303     END-PERFORM                                                          
169403*                                                                         
169503*  MARKERA KUNDER MED FLERA ORDER I ORDERKO-TABELLEN                      
169603     MOVE 1 TO EXPAND-IX                                                  
169703     PERFORM UNTIL EXPAND-IX > ORDERKO-MAX                                
169803                OR (EXPAND-IDDISTR(EXPAND-IX) = SPACE OR ZERO)            
169903                                                                          
170003         MOVE 'HB-KOLLA-FLER-POS5     '  TO WS-CURRENT-SECTION            
170103        IF EXPAND-FLER(EXPAND-IX) = '*'                                   
170203           MOVE +1 TO IX1                                                 
170303                                                                          
170403         MOVE 'HB-KOLLA-FLER-POS6     '  TO WS-CURRENT-SECTION            
170503           PERFORM UNTIL IX1 > ORDERKO-MAX                                
170603           OR (ORDERKO-IDDISTR(IX1)  = EXPAND-IDDISTR(EXPAND-IX)          
170703           AND ORDERKO-IDKUNDNR(IX1) = EXPAND-IDKUNDNR(EXPAND-IX))        
170803                                                                          
170903         MOVE 'HB-KOLLA-FLER-POS7     '  TO WS-CURRENT-SECTION            
171003              ADD +1 TO IX1                                               
171103           END-PERFORM                                                    
171203        END-IF                                                            
171303                                                                          
171403        IF  ORDERKO-IDDISTR(IX1)  = EXPAND-IDDISTR(EXPAND-IX)             
171503        AND ORDERKO-IDKUNDNR(IX1) = EXPAND-IDKUNDNR(EXPAND-IX)            
171603                                                                          
171703         MOVE 'HB-KOLLA-FLER-POS8     '  TO WS-CURRENT-SECTION            
171803           MOVE '*'  TO ORDERKO-FLER(IX1)                                 
171903        END-IF                                                            
172003        ADD +1 TO EXPAND-IX                                               
172103     END-PERFORM                                                          
172203     .                                                                    
172303     EJECT                                                                
172403 Z-FINIT SECTION.                                                         
172503     MOVE 'Z-FINIT '  TO WS-CURRENT-SECTION                               
172603                                                                          
172703     CONTINUE                                                             
172803     .                                                                    
172903     EJECT                                                                
173003*    --- DISPATCHER SECTIONS                                              
173103 S03-RECEIVE-OPEN SECTION.                                                
173203     MOVE 'S03-RECEIVE-OPEN  '            TO WS-CURRENT-SECTION           
173303                                                                          
173403     MOVE 'OPEN'                   TO RECV-KDFUNC                         
173504     MOVE 'CARPARTS.3IV2.REQUASSIGNMENT' TO RECV-ADDISPABS                
173603     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-OPEN-AREA                 
173703                                                                          
173803     IF RECV-KDRC > 0 AND NOT = 20                                        
173903       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
174003       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
174103       DELIMITED BY SIZE INTO ERROR-TEXT                                  
174203       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
174303     END-IF                                                               
174403     .                                                                    
174503     SKIP3                                                                
174603 S03-RECEIVE-MESSAGE SECTION.                                             
174703     MOVE 'S03-RECEIVE-MESSAGE     '      TO WS-CURRENT-SECTION           
174803                                                                          
174903     MOVE 'GET'                      TO RECV-KDFUNC                       
175003     MOVE LENGTH OF RECV-AREA        TO RECV-KVDLEN                       
175103     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-KVDLEN RECV-AREA          
175104                                                                          
175303     IF RECV-KDRC > 1                                                     
175403       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
175503       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
175603       DELIMITED BY SIZE INTO ERROR-TEXT                                  
175703       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
175803     END-IF                                                               
175903     .                                                                    
176003     SKIP3                                                                
176103 S03-RECEIVE-CLOSE SECTION.                                               
176203     MOVE 'S03-RECEIVE-CLOSE       '      TO WS-CURRENT-SECTION           
176303                                                                          
176403     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
176503     CALL WZ01RECV USING RECV-CONTROL-AREA                                
176603                                                                          
176703     IF RECV-KDRC > 0                                                     
176803       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
176903       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
177003       DELIMITED BY SIZE INTO ERROR-TEXT                                  
177103       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
177203     END-IF                                                               
177303     .                                                                    
177403     EJECT                                                                
177503 S04-SEND-OPEN SECTION.                                                   
177603     MOVE 'S04-SEND-OPEN           '      TO WS-CURRENT-SECTION           
177703                                                                          
177803     MOVE 'OPEN'                     TO SEND-KDFUNC                       
177904     MOVE 'CARPARTS.3IV2.RESPASSIGNMENT' TO SEND-ADDISPABS                
178003     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
178103                                                                          
178203     IF SEND-KDRC > 0                                                     
178303       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
178403       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
178503       DELIMITED BY SIZE INTO ERROR-TEXT                                  
178603       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
178703     END-IF                                                               
178803     .                                                                    
178903     SKIP3                                                                
179003 S04-SEND-MESSAGE SECTION.                                                
179103     MOVE 'S04-SEND-MESSAGE        '      TO WS-CURRENT-SECTION           
179203                                                                          
179303     MOVE 'PUT'                      TO SEND-KDFUNC                       
179403*****MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
179503*    -- LENGTH COMPUTED IN C- SECTIONS                                    
179603     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
179703                                                                          
179803     IF SEND-KDRC > 0                                                     
179903       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
180003       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
180103       DELIMITED BY SIZE INTO ERROR-TEXT                                  
180203       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
180303     END-IF                                                               
180403     .                                                                    
180503     SKIP3                                                                
180603 S04-SEND-CLOSE SECTION.                                                  
180703     MOVE 'S04-SEND-CLOSE          '      TO WS-CURRENT-SECTION           
180803                                                                          
180903     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
181003     CALL WZ01SEND USING SEND-CONTROL-AREA                                
181103                                                                          
181203     IF SEND-KDRC > 0                                                     
181303       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
181403       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
181503       DELIMITED BY SIZE INTO ERROR-TEXT                                  
181603       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
181703     END-IF                                                               
181803     .                                                                    
181903* --- IMS SEKTIONER ---                                                   
182003     SKIP3                                                                
182103 IMS-ISRT-MSG-ALT1 SECTION.                                               
182203     MOVE 'IMS-ISRT-MSG-ALT1' TO WS-CURRENT-IMS-SECTION                   
182303                                                                          
182403     MOVE SPACE TO GODK-STATUSKODER                                       
182503     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
182603     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
182703     PERFORM IMS-STATUSKONTROLL                                           
182803     .                                                                    
182903     EJECT                                                                
183003 IMS-GHU-WDQ301              SECTION.                                     
183103     MOVE 'IMS-GU-ORQA01-WDGQ301   '   TO WS-CURRENT-IMS-SECTION          
183203                                                                          
183303     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
183403          DELIMITED BY SIZE INTO SSA1                                     
183503     MOVE '  ' TO GODK-STATUSKODER                                        
183603     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA1 SSA1                    
183703     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
183803     PERFORM IMS-STATUSKONTROLL                                           
183903     .                                                                    
184003                                                                          
184103 IMS-REPL-ORQA-WLORQA01 SECTION.                                          
184203     MOVE 'IMS-REPL-ORQA-WLORQA01'  TO WS-CURRENT-IMS-SECTION             
184303                                                                          
184403     MOVE '  ' TO GODK-STATUSKODER                                        
184503     CALL CBLTDLI USING REPL ORQA-PCB DLI-IO-AREA1                        
184603     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
184703     PERFORM IMS-STATUSKONTROLL                                           
184803     .                                                                    
184903                                                                          
185003 IMS-GHU-ORQI-WLORQI01-WLORQI12 SECTION.                                  
185103     MOVE 'IMS-GHU-ORQI-WLORQI01-WL0RQI12'                                
185203       TO WS-CURRENT-IMS-SECTION                                          
185303                                                                          
185403     STRING 'WLORQI01*D(IDORDER  =' W-IDORDER-X ')'                       
185503          DELIMITED BY SIZE INTO SSA1                                     
185603     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
185703          DELIMITED BY SIZE INTO SSA2                                     
185803     MOVE '  ' TO GODK-STATUSKODER                                        
185903     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA2 SSA1 SSA2               
186003     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
186103     PERFORM IMS-STATUSKONTROLL                                           
186203     .                                                                    
186303                                                                          
186403 IMS-GHU-ORQI-WLORQI01 SECTION.                                           
186503     MOVE 'IMS-GHU-ORQI-WLORQI01 ' TO WS-CURRENT-IMS-SECTION              
186603                                                                          
186703     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
186803          DELIMITED BY SIZE INTO SSA1                                     
186903     MOVE '  ' TO GODK-STATUSKODER                                        
187003     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA2 SSA1                    
187103     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
187203     PERFORM IMS-STATUSKONTROLL                                           
187303     .                                                                    
187403     EJECT                                                                
187503                                                                          
187603 IMS-REPL-ORQI-WLORQI12 SECTION.                                          
187703     MOVE 'IMS-REPL-ORQI-WLORQI12 ' TO WS-CURRENT-IMS-SECTION             
187803                                                                          
187903     MOVE 'WLORQI01*N' TO SSA1                                            
188003     MOVE '    ' TO GODK-STATUSKODER                                      
188103     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA2 SSA1                   
188203     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
188303     PERFORM IMS-STATUSKONTROLL                                           
188403     .                                                                    
188503                                                                          
188603 IMS-GHU-ORQB-WLORQA01 SECTION.                                           
188703     MOVE 'IMS-GHU-ORQB-WLORQA01 '  TO WS-CURRENT-IMS-SECTION             
188803                                                                          
188903     STRING 'WLORQA01(WDQ3BSEQ>=' W-WDQ3BSEQ-MIN-X                        
189003                    '&WDQ3BSEQ<=' W-WDQ3BSEQ-MAX-X                        
189103                    '&IDPRCVAR =' W-Q3BSEQ-MAX-IDPRCVAR ')'               
189203          DELIMITED BY SIZE INTO SSA1                                     
189303     MOVE '  GE' TO GODK-STATUSKODER                                      
189403     CALL CBLTDLI USING GHU ORQB-PCB DLI-IO-AREA1 SSA1                    
189503     MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
189603     PERFORM IMS-STATUSKONTROLL                                           
189703     .                                                                    
189803                                                                          
189903                                                                          
190003 IMS-GHN-ORQB-WLORQA01 SECTION.                                           
190103     MOVE 'IMS-GHN-ORQB-WLORQA01 '  TO WS-CURRENT-IMS-SECTION             
190203                                                                          
190303     STRING 'WLORQA01(WDQ3BSEQ>=' W-WDQ3BSEQ-MIN-X                        
190403                    '&WDQ3BSEQ<=' W-WDQ3BSEQ-MAX-X                        
190503                    '&IDPRCVAR =' W-Q3BSEQ-MAX-IDPRCVAR ')'               
190603          DELIMITED BY SIZE INTO SSA1                                     
190703     MOVE '  GBGE' TO GODK-STATUSKODER                                    
190803     CALL CBLTDLI USING GHN ORQB-PCB DLI-IO-AREA1 SSA1                    
190903     MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
191003     PERFORM IMS-STATUSKONTROLL                                           
191103     .                                                                    
191203                                                                          
191303 IMS-REPL-ORQB-WLORQA01 SECTION.                                          
191403     MOVE 'IMS-REPL-ORQB-WLORQA01'  TO WS-CURRENT-IMS-SECTION             
191503                                                                          
191603     MOVE '  ' TO GODK-STATUSKODER                                        
191703     CALL CBLTDLI USING REPL ORQB-PCB DLI-IO-AREA1                        
191803     MOVE ORQB-STATUS-CODE TO STATUS-WS                                   
191903     PERFORM IMS-STATUSKONTROLL                                           
192003     .                                                                    
192103                                                                          
192203 IMS-GU-WDQ3B1 SECTION.                                                   
192303     MOVE 'IMS-GU-WDQ3B1 '  TO WS-CURRENT-IMS-SECTION                     
192403                                                                          
192503     STRING 'WDQ3B1  (WDQ3B1KY>=' W-WDQ3B1-MIN-X                          
192603                    '&WDQ3B1KY<=' W-WDQ3B1-MAX-X                          
192703                    '&IDPRCVAR =' W-Q3B1-IDPRCVAR ')'                     
192803          DELIMITED BY SIZE INTO SSA1                                     
192903     MOVE '  GBGE' TO GODK-STATUSKODER                                    
193003     CALL CBLTDLI USING GU Q3B1-PCB DLI-IO-Q3B1 SSA1                      
193103     MOVE Q3B1-STATUS-CODE TO STATUS-WS                                   
193203     PERFORM IMS-STATUSKONTROLL                                           
193303     .                                                                    
193403                                                                          
193503 IMS-GHU-WDQ3B1 SECTION.                                                  
193603     MOVE 'IMS-GHU-WDQ3B1 '  TO WS-CURRENT-IMS-SECTION                    
193703                                                                          
193803     STRING 'WDQ3B1  (WDQ3B1KY>=' W-WDQ3B1-MIN-X                          
193903                    '&WDQ3B1KY<=' W-WDQ3B1-MAX-X                          
194003                    '&IDPRCVAR =' W-Q3B1-IDPRCVAR ')'                     
194103          DELIMITED BY SIZE INTO SSA1                                     
194203     MOVE '  GBGE' TO GODK-STATUSKODER                                    
194303     CALL CBLTDLI USING GHU Q3B1-PCB DLI-IO-Q3B1 SSA1                     
194403     MOVE Q3B1-STATUS-CODE TO STATUS-WS                                   
194503                              STATUS-ORDERDEL-Q3B1-WS                     
194603     PERFORM IMS-STATUSKONTROLL                                           
194703     .                                                                    
194803                                                                          
194903 IMS-GN-WDQ3B1 SECTION.                                                   
195003     MOVE 'IMS-GN-WDQ3B1 '  TO WS-CURRENT-IMS-SECTION                     
195103                                                                          
195203     STRING 'WDQ3B1  (WDQ3B1KY>=' W-WDQ3B1-MIN-X                          
195303                    '&WDQ3B1KY<=' W-WDQ3B1-MAX-X                          
195403                    '&IDPRCVAR =' W-Q3B1-IDPRCVAR ')'                     
195503          DELIMITED BY SIZE INTO SSA1                                     
195603     MOVE '  GBGE' TO GODK-STATUSKODER                                    
195703     CALL CBLTDLI USING GN Q3B1-PCB DLI-IO-Q3B1 SSA1                      
195803     MOVE Q3B1-STATUS-CODE TO STATUS-WS                                   
195903     PERFORM IMS-STATUSKONTROLL                                           
196003     .                                                                    
196103                                                                          
196203 IMS-GHN-WDQ3B1 SECTION.                                                  
196303     MOVE 'IMS-GHN-WDQ3B1 '  TO WS-CURRENT-IMS-SECTION                    
196403                                                                          
196503     STRING 'WDQ3B1  (WDQ3B1KY>=' W-WDQ3B1-MIN-X                          
196603                    '&WDQ3B1KY<=' W-WDQ3B1-MAX-X                          
196703                    '&IDPRCVAR =' W-Q3B1-IDPRCVAR ')'                     
196803          DELIMITED BY SIZE INTO SSA1                                     
196903     MOVE '  GBGE' TO GODK-STATUSKODER                                    
197003     CALL CBLTDLI USING GHN Q3B1-PCB DLI-IO-Q3B1 SSA1                     
197103     MOVE Q3B1-STATUS-CODE TO STATUS-WS                                   
197203                              STATUS-ORDERDEL-Q3B1-WS                     
197303     PERFORM IMS-STATUSKONTROLL                                           
197403     .                                                                    
197503                                                                          
197603 IMS-GN-WDQ3B1-DISTR-KUND SECTION.                                        
197703     MOVE 'IMS-GN-WDQ3B1-DISTR-KUND'  TO WS-CURRENT-IMS-SECTION           
197803                                                                          
197903     STRING 'WDQ3B1  (WDQ3B1KY>=' W-WDQ3B1-MIN-X                          
198003                    '&WDQ3B1KY<=' W-WDQ3B1-MAX-X                          
198103                    '&IDDISTR  =' SPAR-IDDISTR-X                          
198203                    '&IDKUNDNR =' SPAR-IDKUNDNR-X                         
198303                    '&IDPRCVAR =' W-Q3B1-IDPRCVAR ')'                     
198403          DELIMITED BY SIZE INTO SSA1                                     
198503     MOVE '  GBGE' TO GODK-STATUSKODER                                    
198603     CALL CBLTDLI USING GN Q3B1-PCB DLI-IO-Q3B1 SSA1                      
198703     MOVE Q3B1-STATUS-CODE TO STATUS-WS                                   
198803     PERFORM IMS-STATUSKONTROLL                                           
198903     .                                                                    
199003                                                                          
199103 IMS-GU-XXKH-WLXXKH11 SECTION.                                            
199203     MOVE 'IMS-GU-XXKH-WLXXKH11  '  TO WS-CURRENT-IMS-SECTION             
199303                                                                          
199403     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
199503          DELIMITED BY SIZE INTO SSA1                                     
199603     STRING 'WLXXKH11(WDGXKEY  =' W-4448-IDPRC-X ')'                      
199703          DELIMITED BY SIZE INTO SSA2                                     
199803     MOVE '  GE' TO GODK-STATUSKODER                                      
199903     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1 SSA2                 
200003     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
200103     PERFORM IMS-STATUSKONTROLL                                           
200203     .                                                                    
200303                                                                          
200403 IMS-GHU-XXKO-WLXXKO11 SECTION.                                           
200503     MOVE 'IMS-GHU-XXKO-WLXXKO11 '  TO WS-CURRENT-IMS-SECTION             
200603                                                                          
200703     STRING 'WLXXKO01(WDGXKEY  =' W-4461-IDHTYP-X ')'                     
200803          DELIMITED BY SIZE INTO SSA1                                     
200903     STRING 'WLXXKO11(WDGXKEY  =' W-4462-KDPRCGRP-X ')'                   
201003          DELIMITED BY SIZE INTO SSA2                                     
201103     MOVE '  GE' TO GODK-STATUSKODER                                      
201203     CALL CBLTDLI USING GHU XXKO-PCB DLI-IO-AREA SSA1 SSA2                
201303     MOVE XXKO-STATUS-CODE TO STATUS-WS                                   
201403     PERFORM IMS-STATUSKONTROLL                                           
201503     .                                                                    
201603                                                                          
201703                                                                          
201803 IMS-REPL-XXKO-WLXXKO11 SECTION.                                          
201903     MOVE 'IMS-REPL-XXKO-WLXXKO11 ' TO WS-CURRENT-IMS-SECTION             
202003                                                                          
202103     MOVE '    ' TO GODK-STATUSKODER                                      
202203     CALL CBLTDLI USING REPL XXKO-PCB DLI-IO-AREA                         
202303     MOVE XXKO-STATUS-CODE TO STATUS-WS                                   
202403     PERFORM IMS-STATUSKONTROLL                                           
202503     .                                                                    
202603                                                                          
202703 IMS-ISRT-4001-WL400101 SECTION.                                          
202803     MOVE 'IMS-ISRT-4001-WL400101 ' TO WS-CURRENT-IMS-SECTION             
202903                                                                          
203003     MOVE 'WL400101 ' TO SSA1                                             
203103     MOVE '    ' TO GODK-STATUSKODER                                      
203203     CALL CBLTDLI USING ISRT 4001-PCB DLI-IO-AREA SSA1                    
203303     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
203403     PERFORM IMS-STATUSKONTROLL                                           
203503     .                                                                    
203603                                                                          
203703 IMS-ISRT-4001-WL400111 SECTION.                                          
203803     MOVE 'IMS-ISRT-4001-WL400111 ' TO WS-CURRENT-IMS-SECTION             
203903                                                                          
204003     STRING 'WL400101(WDGXKEY  =' W-4001-IDHTYP-X ')'                     
204103          DELIMITED BY SIZE INTO SSA1                                     
204203     MOVE 'WL400111 ' TO SSA2                                             
204303     MOVE '    ' TO GODK-STATUSKODER                                      
204403     CALL CBLTDLI USING ISRT 4001-PCB DLI-IO-AREA3 SSA1 SSA2              
204503     MOVE 4001-STATUS-CODE TO STATUS-WS                                   
204603     PERFORM IMS-STATUSKONTROLL                                           
204703     .                                                                    
204803                                                                          
204903 IMS-STATUSKONTROLL SECTION.                                              
205003                                                                          
205103     SET STATUS-IX TO 1                                                   
205203     SEARCH GODK-STATUS                                                   
205303       AT END CALL FELLOG                                                 
205403       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
205503     END-SEARCH                                                           
205603     .                                                                    
205703                                                                          
