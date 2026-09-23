000103*                                                                         
000203******************************************************************        
000303*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0140      *        
000403******************************************************************        
000503*                                                                         
000603 ID DIVISION.                                                             
000703     SKIP2                                                                
000803 PROGRAM-ID.     W4050100.                                                
000903 AUTHOR.         GERRY CARMICHAEL.                                        
001003 DATE-WRITTEN.   90/04/12.                                                
001103                                                                          
001203     REMARKS.                                                             
001303*                                                                         
001403*    FUNKTION.                                                            
001503*        PROGRAMMET LÄSER ORDERHUVUDSREGISTER, ARBETSTABELLEN,            
001603*        DIREKTLEVERANTÖRS SEGMENT OCH ORDERDELSREGISTER OCH              
001703*        VISAR VISS INFORMATION. OM ORDERDELSTATUS ÄR PACKAD (P),         
001803*        ELLER UTSKRIVEN (U) LÄSER PROGRAMMET KOLLIREGISTER OCH           
001903*        LÄGGER UT YTTERLIGARE INFORMATION.                               
002003*                                                                         
002103*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
002203*        PROGRAMMET LÄSER   WLORQI  (WDQ2)                                
002303*                           WLORQA  (WDQ3)                                
002403*                           WDE6                                          
002503*                           WLGMTA  (WDB2)                                
002603*                           WLBETC  (WDB1)                                
002703*                           WDB6                                          
002803*                                                                         
002903*    INDATA.                                                              
003003*        TRANSAKTION: W4T501                                              
003103*        MID:         W4I50101                                            
003203*                                                                         
003303*    UTDATA.                                                              
003403*        MOD:         W4O50101                                            
003503*                                                                         
003603* CHANGE LOG:                                                             
003703*                                                                         
003803                                                                          
003903     SKIP3                                                                
004003 ENVIRONMENT DIVISION.                                                    
004103     EJECT                                                                
004203 DATA DIVISION.                                                           
004303 WORKING-STORAGE SECTION.                                                 
004403*    -- CHECKED BY WY2000                                                 
004503     SKIP3                                                                
004603 77  IDPGM                       PIC X(08)   VALUE 'W4050100'.            
004703                                                                          
004803 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004903                                                                          
005003 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
005103 77  MAX-INDX                    PIC S9(9)  VALUE +13   COMP SYNC.        
005203                                                                          
005303                                                                          
005403 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
005503                                                                          
005603*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005703 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
005803 77  WS-IDDISTR-NUM              PIC 9(4)    VALUE ZERO.                  
005903 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
006003 77  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
006103 77  WS-IDKUNDRF                 PIC X(7)    VALUE SPACE.                 
006203 77  WS-IDPRODNR                 PIC X(7)    VALUE SPACE.                 
006303 77  WS-IDPRODNR-NUM             PIC 9(7)    VALUE ZERO.                  
006403 77  HELP-IDDISTR                PIC 9(4)    VALUE ZERO.                  
006503 77  HELP-IDKUNDNR               PIC 9(6)    VALUE ZERO.                  
006603 77  WS-IDTIDZON                 PIC X(2)    VALUE SPACE.                 
006703 77  WS-KDMATT                   PIC X(1)    VALUE SPACE.                 
006803                                                                          
006903 77  WS-SEK                      PIC X(3)    VALUE 'SEK'.                 
007003                                                                          
007103 01  WS-TID-X.                                                            
007203     03  WS-TID-N                PIC 9(5).                                
007303 01  WS-TID-RED-X.                                                        
007403     03  WS-TID-HH               PIC X(2).                                
007503     03  WS-TID-PKT              PIC X(1).                                
007603     03  WS-TID-MM               PIC X(2).                                
007703                                                                          
007803 77  KVRADER-RAKNARE-WS          PIC S9(5)      VALUE +0   COMP-3.        
007903 77  SUORDV-RAKNARE-WS           PIC S9(9)V9(2) VALUE +0   COMP-3.        
008003 77  SUORDV-EXP-RAKNARE-WS       PIC S9(9)V9(2) VALUE +0   COMP-3.        
008103 77  SUORDV-LOC-RAKNARE-WS       PIC S9(9)V9(2) VALUE +0   COMP-3.        
008203 77  SUORDV-LOCPREL-RAKNARE-WS   PIC S9(9)V9(2) VALUE +0   COMP-3.        
008303 77  VKORDNTO-RAKNARE-WS         PIC S9(6)V9(3) VALUE +0   COMP-3.        
008403 77  VLORDNTO-RAKNARE-WS         PIC S9(4)V9(3) VALUE +0   COMP-3.        
008503 77  KVKOLLI-FAKT-RAKNARE        PIC S9(5)      VALUE +0   COMP-3.        
008603 77  KVKOLLI-LAST-RAKNARE        PIC S9(5)      VALUE +0   COMP-3.        
008703 77  KVKOLPAC-RAKNARE            PIC S9(5)      VALUE +0   COMP-3.        
008803 77  KVORDRAD-PACK-RAKNARE       PIC S9(5)      VALUE +0   COMP-3.        
008903 77  VKORDBTO-RAKNARE            PIC S9(6)V9(3) VALUE +0   COMP-3.        
009003 77  VLORDBTO-RAKNARE            PIC S9(4)V9(3) VALUE +0   COMP-3.        
009103 77  HELP-SUMMA                  PIC S9(9)V9(2) VALUE +0   COMP-3.        
009203                                                                          
009303 77  KVRADER-RAKNARE-ALT         PIC S9(5)      VALUE +0   COMP-3.        
009403 77  SUORDV-RAKNARE-ALT          PIC S9(9)V9(2) VALUE +0   COMP-3.        
009503 77  SUORDV-LOC-RAKNARE-ALT      PIC S9(9)V9(2) VALUE +0   COMP-3.        
009603 77  SUORDV-LOCPREL-RAKNARE-ALT  PIC S9(9)V9(2) VALUE +0   COMP-3.        
009703 77  VKORDNTO-RAKNARE-ALT        PIC S9(6)V9(3) VALUE +0   COMP-3.        
009803 77  VLORDNTO-RAKNARE-ALT        PIC S9(4)V9(3) VALUE +0   COMP-3.        
009903                                                                          
010003 01  WS-TEDDI.                                                            
010103     03 FILLER                   PIC X(3) VALUE SPACE.                    
010203     03 WS-KDVALISO              PIC X(3).                                
010303     03 FILLER                   PIC X(5) VALUE SPACE.                    
010403                                                                          
010503 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010603     88  NYCKLAR-OK                          VALUE 'J'.                   
010703     88  NYCKLAR-FEL                         VALUE 'N'.                   
010803                                                                          
010903 77  ARB-SW                      PIC X       VALUE 'J'.                   
011003     88  ARB-OK                              VALUE 'J'.                   
011103     88  ARB-FEL                             VALUE 'N'.                   
011203                                                                          
011303 77  ALLT-SW                     PIC X       VALUE 'J'.                   
011403     88  ALLT-OK                             VALUE 'J'.                   
011503                                                                          
011603 77  PROD-INGANG-SW              PIC X       VALUE 'J'.                   
011703     88  PROD-INGANG                         VALUE 'J'.                   
011803                                                                          
011903 77  DKO-INGANG-SW               PIC X       VALUE 'J'.                   
012003     88  DKO-INGANG                          VALUE 'J'.                   
012103                                                                          
012203*                                                                         
012303*                                                                         
012403 01  TEST-IDDISTR             PIC S9(5) COMP-3.                           
012503*    ----DISTR-DEALER-PRICE----                                           
012603*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
012703     EJECT                                                                
012803*      --- VALID IDDC CODES                                               
012903*                                                                         
013003*01    -COPY WWDCKONS                                                     
013103       EJECT                                                              
013203                                                                          
013303 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013403     88  EGEN-MID                            VALUE '4501'.                
013503     88  GODK-MID                            VALUE '4501' '4502'          
013603                                                   '4503' '4504'          
013703                                                   '4505' '4506'          
013803                                                   '4507' '4508'          
013903                                                   '4509'.                
014003     EJECT                                                                
014103*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014203 01  GENERELLA-SUBPROGRAM.                                                
014303     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014403     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
014503     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014603     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014703     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014803     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
014903     EJECT                                                                
015003*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015103*01 -COPY WMSGINIT                                                        
015203     EJECT                                                                
015303*    --- PARAMETRAR TILL SUBPROGRAM WWOMVAND                              
015403*01 -COPY WWOMVAND                                                        
015503     EJECT                                                                
015603 01  SPAR-AREOR.                                                          
015703     03 ODEL-IDDC-SPAR           PIC X(2)    VALUE SPACE.                 
015803     03 ODEL-IDPRODNR-SPAR       PIC S9(7)   VALUE ZERO.                  
015903     03 ODEL-IDLEVNR-SPAR        PIC  X(5)   VALUE SPACE.                 
016003     03 VORD-IDPRODNR-SPAR       PIC S9(7)   VALUE ZERO.                  
016103     EJECT                                                                
016203*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016303*   -COPY WMEDAREA                                                        
016403     SKIP3                                                                
016503 01  MESSAGE-CODES.                                                       
016603     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
016703     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016803     03  INF-LINES-MISSING-CL    PIC X(3)    VALUE '028'.                 
016903     03  INF-ORDER-ANNULLED      PIC X(3)    VALUE '052'.                 
017003     03  INF-ORDER-EJ-AVSLUT     PIC X(3)    VALUE '053'.                 
017103     03  INF-ORDER-EJ-KLAR       PIC X(3)    VALUE '791'.                 
017203     03  INF-ORDER-MISSING       PIC X(3)    VALUE '701'.                 
017303     03  INF-ORDERHEAD-MISSING   PIC X(3)    VALUE '417'.                 
017403     03  INF-ORDERINFO-BORTTAGEN PIC X(3)    VALUE '415'.                 
017503     03  INF-ORDERLINES-MISSING  PIC X(3)    VALUE '029'.                 
017603     03  INF-TRANSPORT-EJ-BOKAD  PIC X(3)    VALUE '055'.                 
017703     EJECT                                                                
017803*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017903*                                                                         
018003 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018103     SKIP3                                                                
018203*01  MID -COPY W4I50101                                                   
018303     EJECT                                                                
018403 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018503     SKIP3                                                                
018603*01  -COPY WMSGAREA                                                       
018703     EJECT                                                                
018803*    03  MOD -COPY W4O50101   -RED MSG-AREA.                              
018903     EJECT                                                                
019003 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019103     SKIP3                                                                
019203*01  -COPY WMFSAREA                                                       
019303     EJECT                                                                
019403*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019503*                                                                         
019603 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019703     SKIP3                                                                
019803 01  NYCKLAR-TILL-DLI.                                                    
019903     03  W-WDQ2CSEQ-X.                                                    
020003         05  W-SEQC-IDDISTR      PIC S9(5)    VALUE ZERO COMP-3.          
020103         05  W-SEQC-IDKUNDNR     PIC S9(7)    VALUE ZERO COMP-3.          
020203         05  W-SEQC-IDKUNDRF     PIC X(10)    VALUE SPACE.                
020303     03  W-WDQ211KY-X.                                                    
020403         05  W-DIRL-IDDC         PIC X(2)     VALUE SPACE.                
020503         05  W-DIRL-IDLEVNR      PIC  X(5)    VALUE SPACE.                
020603     03  W-IDDC-X.                                                        
020703         05  W-ARB-IDDC          PIC X(2)     VALUE SPACE.                
020803     03  W-IDPRODNR-X.                                                    
020903         05  W-VORD-IDPRODNR     PIC S9(7)    VALUE ZERO COMP-3.          
021003     03  W-IDGMT-X.                                                       
021103         05  W-IDDISTR-GMT       PIC S9(5)    VALUE ZERO COMP-3.          
021203         05  W-IDKUNDNR-GMT      PIC S9(7)    VALUE ZERO COMP-3.          
021303     03  W-IDGMT-MIN-X.                                                   
021403         05  W-IDDISTR-GMT-MIN   PIC S9(5)    VALUE ZERO COMP-3.          
021503         05  W-IDKUNDNR-GMT-MIN  PIC S9(7)    VALUE ZERO COMP-3.          
021603     03  W-IDGMT-MAX-X.                                                   
021703         05  W-IDDISTR-GMT-MAX   PIC S9(5)    VALUE ZERO COMP-3.          
021803         05  W-IDKUNDNR-GMT-MAX  PIC S9(7)    VALUE ZERO COMP-3.          
021903     03  W-WDB101KY-X.                                                    
022003         05  W-WDB1-IDPARTNR     PIC X(9)     VALUE SPACE.                
022103         05  W-WDB1-IDFTG        PIC 9(2)     VALUE ZERO.                 
022203     03  W-IDDISTR-X.                                                     
022303         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
022403     03  W-IDKUNDNR-X.                                                    
022503         05  W-IDKUNDNR          PIC S9(7)    VALUE ZERO COMP-3.          
022603     03  W-WDQ3DSEQ-MIN-X.                                                
022703         05  W-Q3DSEQ-IDPRODNR-MIN   PIC S9(7) VALUE ZERO COMP-3.         
022803         05  W-Q3DSEQ-IDPLKLST-MIN   PIC S9(3) VALUE ZERO COMP-3.         
022903     03  W-WDQ3DSEQ-MAX-X.                                                
023003         05  W-Q3DSEQ-IDPRODNR-MAX   PIC S9(7) VALUE ZERO COMP-3.         
024003         05  W-Q3DSEQ-IDPLKLST-MAX   PIC S9(3) VALUE ZERO COMP-3.         
024103     03  W-WDQ301KY-MIN-X.                                                
024203         05  W-ODEL-IDORDER-MIN  PIC S9(7)    VALUE ZERO COMP-3.          
024303         05  W-ODEL-IDDC-MIN     PIC X(2)     VALUE SPACE.                
024403         05  W-ODEL-IDPRODNR-MIN PIC S9(7)    VALUE ZERO COMP-3.          
024503         05  W-ODEL-IDPLKLST-MIN PIC S9(3)    VALUE ZERO COMP-3.          
024603     03  W-WDQ301KY-MAX-X.                                                
024703         05  W-ODEL-IDORDER-MAX  PIC S9(7)    VALUE ZERO COMP-3.          
024803         05  W-ODEL-IDDC-MAX     PIC X(2)     VALUE SPACE.                
024903         05  W-ODEL-IDPRODNR-MAX PIC S9(7)    VALUE ZERO COMP-3.          
025003         05  W-ODEL-IDPLKLST-MAX PIC S9(3)    VALUE ZERO COMP-3.          
025103                                                                          
025203     03  W-IDDC-B6-X.                                                     
025303         05 W-IDDC-B6                  PIC X(2).                          
025403                                                                          
025503     03  W-IDDC-B6-DDGS-X.                                                
025603         05 W-IDDC-B6-DDGS             PIC X(2).                          
025703                                                                          
025803*    --- STATUS-KOD FRÅN IMS                                              
025903 01  STATUS-WS                   PIC XX.                                  
026003     88  SEGMENT-FINNS                       VALUE '  '.                  
026103     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026203     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026303     88  BASEN-SLUT                          VALUE 'GB'.                  
026403     SKIP2                                                                
026503 01  GODK-STATUSKODER.                                                    
026603     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026703     SKIP3                                                                
026803 01  SSA1                        PIC X(128).                              
026903 01  SSA2                        PIC X(64).                               
027003     EJECT                                                                
027103 01   MEDDELANDE.                                                         
027203*                                                                         
027303   03 INTERNMED.                                                          
027403     05 INTERNMED-S            PIC X(22) VALUE                            
027503                                        'INTERN SPECIALORDER   '.         
027603     05 INTERNMED-GB           PIC X(22) VALUE                            
027703                                        'INTERNAL SPECIAL ORDER'.         
027803   03 FILLER REDEFINES INTERNMED.                                         
027903     05 INTERN-MED             PIC X(22) OCCURS 2.                        
028003*                                                                         
028103   03 SOFTMED.                                                            
028203     05 SOFTMED-S              PIC X(22) VALUE                            
028303                                        'SOFTWARE ORDER      '.           
028403     05 SOFTMED-GB             PIC X(22) VALUE                            
028503                                        'SOFTWARE ORDER      '.           
028603   03 FILLER REDEFINES SOFTMED.                                           
028703     05 SOFT-MED               PIC X(22) OCCURS 2.                        
028803*                                                                         
028903   03 SPAERRMED.                                                          
029003     05 SPAERRMED-S            PIC X(22) VALUE                            
029103                                        'BETALARE STOPPAD      '.         
029203     05 SPAERRMED-GB           PIC X(22) VALUE                            
029303                                        'CUSTOMER STOPPED      '.         
029403   03 FILLER REDEFINES SPAERRMED.                                         
029503     05 SPAERR-MED             PIC X(22) OCCURS 2.                        
029603     EJECT                                                                
029703*                            IMS FUNKTIONSKODER                           
029803*01  -COPY W0003                                                          
029903     EJECT                                                                
030003******************************************************************        
030103*                                                                         
030203*        ARBETS-AREOR TILL IO-AREORNA                                     
030303*                                                                         
030403*    ---  DLI INPUT-OUTPUT AREA 1                                         
030503*    ---  DLI-IO-AREA                                                     
030603                                                                          
030703 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-OHUV'.             
030803 01  DLI-IO-AREA-OHUV.                                                    
030903*    03  WLORQI01    -COPY WDQ201                                         
031003     EJECT                                                                
031103                                                                          
031203 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-DIRL'.             
031303 01  DLI-IO-AREA-DIRL.                                                    
031403*    03  WLORQI11.   -COPY WDQ211                                         
031503     EJECT                                                                
031603                                                                          
031703 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARB '.             
031803 01  DLI-IO-AREA-ARB.                                                     
031903*    03  WLORQI12.   -COPY WDQ212                                         
032003     EJECT                                                                
032103                                                                          
032204 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-LOR '.             
032304 01  DLI-IO-AREA-LOR.                                                     
032404*    03  WLORQI21.   -COPY WDQ221                                         
032504     EJECT                                                                
032604                                                                          
032704 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ODEL'.             
032804 01  DLI-IO-AREA-ODEL.                                                    
032904*    03  WLORQA01.   -COPY WDQ301                                         
033004     EJECT                                                                
033104                                                                          
033204 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-VORD'.             
033304 01  DLI-IO-AREA-VORD.                                                    
033404*    03              -COPY WDE601                                         
033504     EJECT                                                                
033604                                                                          
033704 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-GMTA'.             
033804 01  DLI-IO-AREA-GMTA.                                                    
033904*    03  WLGMTA01    -COPY WDB201                                         
034004                                                                          
034104     EJECT                                                                
034204 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-BETC'.             
034304 01  DLI-IO-AREA-BETC.                                                    
034404*    03  WLBETC01    -COPY WDB101                                         
034504                                                                          
034604 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
034704 01   DLI-IO-AREA-B601.                                                   
034804*     03  -COPY WDB601                                                    
034904                                                                          
035004 01  FILLER               PIC X(16)   VALUE 'WDB601 DDGS'.                
035104 01   DLI-IO-AREA-B601-DDGS.                                              
035204*     03  -COPY WDB601   -PRE DDGS-                                       
035304                                                                          
035404                                                                          
035504     EJECT                                                                
035604 01  FILLER                  PIC X(16)   VALUE 'WSEC-AREA  '.             
035704*01  -COPY WSECAREA                                                       
035804     EJECT                                                                
035904 01  FILLER                  PIC X(16)   VALUE 'W402W001   '.             
036004*   -COPY W402W001                                                        
036104     EJECT                                                                
036204 LINKAGE SECTION.                                                         
036304                                                                          
036404*01  -COPY W0009      -PRE MSG-                                           
036504     EJECT                                                                
036604*01  -COPY W0008      -PRE USEA-                                          
036704     05  FILLER                  PIC X.                                   
036804     EJECT                                                                
036904*01  -COPY W0008      -PRE ORQI-                                          
037004     05  FILLER                  PIC X.                                   
037104     EJECT                                                                
037204*01  -COPY W0008      -PRE ORQA-                                          
037304     05  FILLER                  PIC X.                                   
037404     EJECT                                                                
037504*01  -COPY W0008      -PRE ORQAD-                                         
037604     05  FILLER                  PIC X.                                   
037704     EJECT                                                                
037804*01  -COPY W0008      -PRE WDE6-                                          
037904     05  FILLER                  PIC X.                                   
038004     EJECT                                                                
038104*01  -COPY W0008      -PRE GMTA-                                          
038204     05  FILLER                  PIC X.                                   
038304     EJECT                                                                
038404*01  -COPY W0008      -PRE BETC-                                          
038504     05  FILLER                  PIC X.                                   
038604     EJECT                                                                
038704*01  -COPY W0008      -PRE WDB6-                                          
038804     05  FILLER                  PIC X.                                   
038904*                                                                         
039004*                                                                         
039104 PROCEDURE DIVISION  USING MSG-PCB     USEA-PCB                           
039204                           ORQI-PCB                                       
039304                           ORQA-PCB    ORQAD-PCB                          
039404                           WDE6-PCB    GMTA-PCB                           
039504                           BETC-PCB                                       
040003                           WDB6-PCB.                                      
040103     ENTRY 'DLITCBL' USING MSG-PCB     USEA-PCB                           
040203                           ORQI-PCB                                       
040303                           ORQA-PCB    ORQAD-PCB                          
040403                           WDE6-PCB    GMTA-PCB                           
040503                           BETC-PCB                                       
040603                           WDB6-PCB.                                      
040703                                                                          
040803                                                                          
040903     PERFORM IMS-GET-MSG                                                  
041003     IF SEGMENT-FINNS                                                     
041103       PERFORM A-INIT                                                     
041203       PERFORM C-KOLLA-NYCKLAR                                            
041303       IF NYCKLAR-OK                                                      
041403          PERFORM D-LAES-BASEN                                            
041503          IF ALLT-OK                                                      
041603            PERFORM F-REDIGERA-BILDEN                                     
041703          END-IF                                                          
041803       END-IF                                                             
041903                                                                          
042003       IF NOT ALLT-OK                                                     
042103         CALL WMEDKONV   USING MED-WMEDAREA                               
042203         IF MED-IDMFSFEL NUMERIC AND                                      
042303            MED-IDMFSFEL > 0                                              
042403           MOVE MED-MFSFEL TO    MOD-TEMFSFEL                             
042503         ELSE                                                             
042603           IF MED-IDMFSINF NUMERIC AND                                    
042703              MED-IDMFSINF > 0                                            
042803             MOVE MED-MFSINF TO    MOD-TEMFSINF                           
042903           END-IF                                                         
043003         END-IF                                                           
043103       END-IF                                                             
043203                                                                          
043303       PERFORM IMS-INSERT-MSG                                             
043403     END-IF                                                               
043503                                                                          
043603     MOVE ZERO TO RETURN-CODE                                             
043703     GOBACK                                                               
043803     .                                                                    
043903     EJECT                                                                
044003 A-INIT SECTION.                                                          
044103                                                                          
044203     IF MSG-DUBBLA-TRANSKODER                                             
044303       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I50101                 
044403       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
044503       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
044603     ELSE                                                                 
044703       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I50101                  
044803       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
044903       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
045003     END-IF                                                               
045103                                                                          
045203     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
045303     MOVE MSG-IDPFK TO MFS-IDPFK                                          
045403     MOVE MFS-IDTRANS TO W-IDTRANS                                        
045503     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O50101 + 4                        
045603                                                                          
045703     MOVE LOW-VALUE TO MSG-AREA                                           
045803     MOVE 'W4O501N1' TO MFS-IDMOD                                         
045903     MOVE '4501' TO MOD-IDTRANS                                           
046003     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
046103                                                                          
046203     IF NOT EGEN-MID                                                      
046303       MOVE SPACE TO MFS-KDTRTYP                                          
046403       MOVE '7' TO MFS-IDPFK                                              
046503     END-IF                                                               
046603                                                                          
046703     IF ENGLISH-TEXT                                                      
046803       MOVE +2 TO SPRAK-IX                                                
046903     ELSE                                                                 
047003       MOVE +1 TO SPRAK-IX                                                
047103     END-IF                                                               
047203     MOVE +0   TO VKORDNTO-RAKNARE-WS                                     
047303     MOVE  LOW-VALUE         TO W-IDGMT-MIN-X                             
047403                                                                          
047503     MOVE HIGH-VALUE         TO W-IDGMT-MAX-X                             
047603     .                                                                    
047703     EJECT                                                                
047803 C-KOLLA-NYCKLAR SECTION.                                                 
048003     MOVE ALL '+'           TO MSGI-WMSGINIT                              
048103     MOVE '001'             TO MSGI-KDCALL                                
048203     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
048303     MOVE '4501'            TO MSGI-IDTRANS                               
048403     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
048503     IF EGEN-MID                                                          
048603        MOVE MID-IDPRODNR-IN    TO MSGI-IDPRODNR                          
048703        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
048803        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
048903        IF MID-IDKUNDRF-IN      NOT = ALL '+'                             
049003           MOVE SPACE           TO MSGI-IDKUNDRF                          
049103           MOVE MID-IDKUNDRF-IN TO MSGI-IDKUNDRF                          
049203        END-IF                                                            
049303        MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                           
049403        MOVE MID-IDKOLLI-IN     TO MSGI-IDKOLLI                           
049503     END-IF                                                               
049603     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
049703     MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                                
049803     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
049903                                                                          
050003     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
050103                                                                          
050203     MOVE JA                TO NYCKLAR-SW                                 
050303                               DKO-INGANG-SW                              
050403     MOVE NEJ               TO PROD-INGANG-SW                             
050503                               ARB-SW                                     
050603                                                                          
050703     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
050803                             MOD-IDKUNDNR-IN                              
050903                             MOD-IDKUNDRF-IN                              
051003                             MOD-IDARTNR-IN                               
051103                             MOD-IDKOLLI-IN                               
051203                             MOD-IDPRODNR-IN                              
051303                             MOD-IDDC-IN                                  
051403                                                                          
051503     MOVE LOW-VALUE       TO W-WDQ2CSEQ-X                                 
051603                             W-WDQ211KY-X                                 
051703                             W-IDDC-X                                     
051803                             W-IDPRODNR-X                                 
051903                             W-WDQ3DSEQ-MIN-X                             
052003                             W-WDQ301KY-MIN-X                             
052103                                                                          
052203     MOVE HIGH-VALUE      TO W-WDQ301KY-MAX-X                             
052303                             W-WDQ3DSEQ-MAX-X                             
052403                                                                          
052503     PERFORM CB-KOLLA-IDDISTR                                             
052603     PERFORM CC-KOLLA-IDKUNDNR                                            
052703     PERFORM CD-KOLLA-IDKUNDRF                                            
052803     PERFORM CE-KOLLA-IDPRODNR                                            
052903     PERFORM CF-KOLLA-IDDC                                                
053003     PERFORM CG-KOLLA-IDARTNR                                             
053103     PERFORM CH-KOLLA-IDKOLLI                                             
053203                                                                          
053303     MOVE      DCS-IDDC          TO        MOD-IDDC-UT                    
053403                                                                          
053503     MOVE      MSGI-IDDISTR      TO        MOD-IDDISTR-UT                 
053603     INSPECT MOD-IDDISTR-UT      REPLACING LEADING ZERO BY SPACE          
053703     MOVE      MSGI-IDKUNDNR     TO        MOD-IDKUNDNR-UT                
053803     INSPECT MOD-IDKUNDNR-UT     REPLACING LEADING ZERO BY SPACE          
053903     IF MOD-IDKUNDNR-UT = ALL SPACE AND                                   
054003        NOT PROD-INGANG                                                   
054103       MOVE '     0' TO MOD-IDKUNDNR-UT                                   
054203     END-IF                                                               
054303     MOVE      MSGI-IDKUNDRF     TO        MOD-IDKUNDRF-UT                
054403     INSPECT MOD-IDKUNDRF-UT     REPLACING LEADING ZERO BY SPACE          
054503     IF NOT DKO-INGANG                                                    
054603       MOVE      MSGI-IDPRODNR     TO        MOD-IDPRODNR-UT              
054703       INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE            
054803     END-IF                                                               
054903                                                                          
055003     IF GODK-MID                                                          
055103        CONTINUE                                                          
055203     ELSE                                                                 
055303       MOVE MFS-RENSA-FAELT      TO        MOD-IDARTNR-UT                 
055403                                           MOD-IDKOLLI-UT                 
055503                                           MOD-IDPRODNR-UT                
055603     END-IF                                                               
055703                                                                          
055803     IF NYCKLAR-FEL                                                       
055903       MOVE    ERR-WRONG-KEY TO    MED-IDMFSFEL                           
056003       CALL    WMEDKONV      USING MED-WMEDAREA                           
056103       MOVE    MED-MFSFEL    TO    MOD-TEMFSFEL                           
056203       PERFORM MFS-RENSA-FAELT-UT                                         
056303     END-IF                                                               
056403     .                                                                    
056503     EJECT                                                                
056603 CB-KOLLA-IDDISTR SECTION.                                                
056703                                                                          
056803     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
056903        MOVE MSGI-IDDISTR    TO TEST-IDDISTR                              
057003     ELSE                                                                 
057103        MOVE ZERO            TO TEST-IDDISTR                              
057203     END-IF                                                               
057303                                                                          
057403     IF MID-IDDISTR-IN   NOT = ALL '+'                                    
057503       MOVE '7' TO MFS-IDPFK                                              
057603       MOVE SPACE TO MFS-KDTRTYP                                          
057703     END-IF                                                               
057803                                                                          
057903     IF MID-IDPRODNR-IN = ALL '+'                                         
058003       IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                    
058103         MOVE MSGI-IDDISTR TO WS-IDDISTR-NUM                              
058203         MOVE WS-IDDISTR-NUM TO W-SEQC-IDDISTR                            
058303                                W-IDDISTR                                 
058403                                W-IDDISTR-GMT                             
058503                                W-IDDISTR-GMT-MIN                         
058603                                W-IDDISTR-GMT-MAX                         
058703         MOVE JA TO DKO-INGANG-SW                                         
058803       ELSE                                                               
058903         MOVE NEJ TO NYCKLAR-SW                                           
059003       END-IF                                                             
059103     END-IF                                                               
059203                                                                          
059303     MOVE SPACE              TO MOD-TEDDI                                 
059403     .                                                                    
059503     EJECT                                                                
059603                                                                          
059703 CC-KOLLA-IDKUNDNR SECTION.                                               
059803                                                                          
059903     IF MID-IDKUNDNR-IN         NOT = ALL '+'                             
060003       MOVE '7'                 TO MFS-IDPFK                              
061003       MOVE SPACE               TO MFS-KDTRTYP                            
061103     END-IF                                                               
061203                                                                          
061303     IF MID-IDPRODNR-IN         = ALL '+'                                 
061403       IF MSGI-IDKUNDNR NUMERIC                                           
061503         MOVE MSGI-IDKUNDNR     TO WS-IDKUNDNR-NUM                        
061603         MOVE WS-IDKUNDNR-NUM   TO W-SEQC-IDKUNDNR                        
061703                                   W-IDKUNDNR                             
061803                                   W-IDKUNDNR-GMT                         
061903       ELSE                                                               
062003         MOVE NEJ TO NYCKLAR-SW                                           
062103       END-IF                                                             
062203     END-IF                                                               
062303     .                                                                    
062403     EJECT                                                                
062503                                                                          
062603 CD-KOLLA-IDKUNDRF SECTION.                                               
062703                                                                          
062803     IF MID-IDKUNDRF-IN       NOT = ALL '+'                               
062903       MOVE '7'               TO MFS-IDPFK                                
063003       MOVE SPACE             TO MFS-KDTRTYP                              
063103     END-IF                                                               
063203                                                                          
063303     IF MID-IDPRODNR-IN            = ALL '+'                              
063403       IF MSGI-IDKUNDRF(1:7)       NUMERIC AND                            
063503          MSGI-IDKUNDRF(1:7)       > ZERO                                 
063603         MOVE MSGI-IDKUNDRF(1:7)   TO W-SEQC-IDKUNDRF                     
063703       ELSE                                                               
063803         MOVE NEJ                  TO NYCKLAR-SW                          
063903       END-IF                                                             
064003     END-IF                                                               
064103     .                                                                    
064203     EJECT                                                                
064303                                                                          
064403 CE-KOLLA-IDPRODNR SECTION.                                               
064503                                                                          
064603     IF MID-IDPRODNR-IN       NOT = ALL '+'                               
064703       MOVE '7' TO MFS-IDPFK                                              
064803       MOVE SPACE TO MFS-KDTRTYP                                          
064903     END-IF                                                               
065003                                                                          
065103     IF MID-IDPRODNR-IN         NOT = ALL '+'                             
065203       IF MSGI-IDPRODNR NUMERIC AND MSGI-IDPRODNR > ZERO                  
065303         MOVE MSGI-IDPRODNR     TO WS-IDPRODNR-NUM                        
065403         MOVE WS-IDPRODNR-NUM   TO W-Q3DSEQ-IDPRODNR-MIN                  
065503                                   W-Q3DSEQ-IDPRODNR-MAX                  
065603                                   W-ODEL-IDPRODNR-MIN                    
065703                                   W-ODEL-IDPRODNR-MAX                    
065803         MOVE JA   TO PROD-INGANG-SW                                      
065903         MOVE NEJ  TO DKO-INGANG-SW                                       
066003       ELSE                                                               
066103         MOVE NEJ TO NYCKLAR-SW                                           
066203       END-IF                                                             
066303     END-IF                                                               
066403     .                                                                    
066503     EJECT                                                                
066603 CF-KOLLA-IDDC SECTION.                                                   
066703                                                                          
066803     IF EGEN-MID                                                          
066903        IF MID-IDDC-IN     = ALL '+'                                      
067003          MOVE MID-IDDC-UT TO W-IDDC-B6                                   
067103        ELSE                                                              
067203          MOVE MID-IDDC-IN TO W-IDDC-B6                                   
067303          MOVE '7'        TO MFS-IDPFK                                    
067403          MOVE SPACE      TO MFS-KDTRTYP                                  
067503        END-IF                                                            
067603     ELSE                                                                 
067703        MOVE MSGI-IDDC       TO W-IDDC-B6                                 
067803     END-IF                                                               
067903     PERFORM IMS-GU-WDB601                                                
068003                                                                          
068103     IF NYCKLAR-OK                                                        
068203       IF DCS-KDDC = SPACE OR DCS-CDC-TR                                  
068303         MOVE NEJ        TO NYCKLAR-SW                                    
068403       ELSE                                                               
068503         IF DCS-DDC                                                       
068603           MOVE WC-CDC-SE TO W-ARB-IDDC                                   
068703         ELSE                                                             
068803           MOVE DCS-IDDC  TO W-ARB-IDDC                                   
068903         END-IF                                                           
069003       END-IF                                                             
069103     END-IF                                                               
069203     .                                                                    
069303     EJECT                                                                
069403                                                                          
069503 CG-KOLLA-IDARTNR SECTION.                                                
069603                                                                          
069703     IF MID-IDARTNR-IN = ALL '+'                                          
069803       MOVE MID-IDARTNR-UT TO MOD-IDARTNR-UT                              
069903     ELSE                                                                 
070003       MOVE MID-IDARTNR-IN TO MOD-IDARTNR-UT                              
070103       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
070203     END-IF                                                               
070303     .                                                                    
070403     EJECT                                                                
070503 CH-KOLLA-IDKOLLI SECTION.                                                
070603                                                                          
070703     IF MID-IDKOLLI-IN = ALL '+'                                          
070803       MOVE    MID-IDKOLLI-UT TO MOD-IDKOLLI-UT                           
070903     ELSE                                                                 
071003       MOVE    MID-IDKOLLI-IN TO MOD-IDKOLLI-UT                           
071103       INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE             
071203     END-IF                                                               
071303     .                                                                    
071403     EJECT                                                                
071503 D-LAES-BASEN SECTION.                                                    
071603                                                                          
071703     MOVE JA TO ALLT-SW                                                   
071803     IF NOT PROD-INGANG                                                   
071903       PERFORM IMS-GU-ORQI01-M-Q2CSEQ                                     
072003       IF SEGMENT-FINNS                                                   
073003         IF MSG-SIGNON-USERID(1:2) = 'A0' OR 'TI' OR 'PC'                 
073103           MOVE OHUV-IDORDER    TO MOD-IDORDER                            
073203         ELSE                                                             
073303           MOVE MFS-RENSA-FAELT TO MOD-IDORDER                            
073403         END-IF                                                           
073503         MOVE OHUV-IDORDER TO W-ODEL-IDORDER-MIN                          
073603                              W-ODEL-IDORDER-MAX                          
073703         PERFORM IMS-GU-ORQA01                                            
073803         PERFORM DB-KOLLA-BEHOERIGHET                                     
073903         IF ALLT-OK                                                       
074003           PERFORM DA-LAES-OCH-REDIGERA                                   
074103         END-IF                                                           
074203       ELSE                                                               
074303         MOVE MFS-RENSA-FAELT   TO MOD-IDORDER                            
074403         MOVE INF-ORDER-MISSING TO MED-IDMFSINF                           
074503         MOVE NEJ               TO ALLT-SW                                
074603       END-IF                                                             
074703     ELSE                                                                 
074803       PERFORM IMS-GU-ORQA01-M-Q3DSEQ                                     
074903       IF SEGMENT-FINNS                                                   
075003         MOVE ODEL-IDORDER  TO W-ODEL-IDORDER-MIN                         
075103                               W-ODEL-IDORDER-MAX                         
075203         IF MSG-SIGNON-USERID(1:2) = 'A0' OR 'TI' OR 'PC'                 
075303           MOVE ODEL-IDORDER    TO MOD-IDORDER                            
075403         ELSE                                                             
075503           MOVE MFS-RENSA-FAELT TO MOD-IDORDER                            
075603         END-IF                                                           
075703         MOVE ODEL-IDDISTR  TO HELP-IDDISTR                               
075803                               W-SEQC-IDDISTR                             
075903         MOVE HELP-IDDISTR  TO WS-IDDISTR                                 
076003         MOVE HELP-IDDISTR  TO MOD-IDDISTR-UT                             
076103                               WS-IDDISTR-NUM                             
076203         MOVE WS-IDDISTR-NUM TO   MSGI-IDDISTR                            
076303                                  W-IDDISTR-GMT                           
076403                                  W-IDDISTR-GMT-MIN                       
076503                                  W-IDDISTR-GMT-MAX                       
076603         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
076703         MOVE ODEL-IDKUNDNR TO HELP-IDKUNDNR                              
076803                               W-SEQC-IDKUNDNR                            
076903                               WS-IDKUNDNR-NUM                            
077003         MOVE WS-IDKUNDNR-NUM TO  MSGI-IDKUNDNR                           
077103                                  W-IDKUNDNR-GMT                          
077203         MOVE HELP-IDKUNDNR TO MOD-IDKUNDNR-UT                            
077303         INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE          
077403         IF MOD-IDKUNDNR-UT = ALL SPACE                                   
077503           MOVE '     0' TO MOD-IDKUNDNR-UT                               
077603         END-IF                                                           
077703         MOVE ODEL-IDKUNDRF TO MOD-IDKUNDRF-UT                            
077803                               W-SEQC-IDKUNDRF                            
077903                               MSGI-IDKUNDRF                              
078003                                                                          
078103         MOVE '001'             TO MSGI-KDCALL                            
078203         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
078303         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
078403                                                                          
078503         INSPECT MOD-IDKUNDRF-UT REPLACING LEADING ZERO BY SPACE          
078603         MOVE ODEL-IDDC     TO W-ARB-IDDC                                 
078703         PERFORM IMS-GU-ORQI01-M-Q2CSEQ                                   
078803         IF SEGMENT-FINNS                                                 
078903           PERFORM DB-KOLLA-BEHOERIGHET                                   
079003           IF ALLT-OK                                                     
079103             PERFORM DA-LAES-OCH-REDIGERA                                 
079203           END-IF                                                         
079303         ELSE                                                             
079403           MOVE INF-ORDER-MISSING TO MED-IDMFSINF                         
079503           MOVE NEJ               TO ALLT-SW                              
079603         END-IF                                                           
079703       ELSE                                                               
079803         MOVE MFS-RENSA-FAELT   TO MOD-IDORDER                            
079903         MOVE INF-ORDER-MISSING TO MED-IDMFSINF                           
080003         MOVE NEJ               TO ALLT-SW                                
080103       END-IF                                                             
080203     END-IF                                                               
080303     .                                                                    
080403     EJECT                                                                
080503 DA-LAES-OCH-REDIGERA SECTION.                                            
080603                                                                          
080703     IF OHUV-FLBORT = 'J'                                                 
080803       PERFORM IMS-GNP-ORQI12                                             
080903       PERFORM DAA-KOLLA-ORQI12-SEGMENT                                   
081003     ELSE                                                                 
081103       IF OHUV-KDTPOTYP = ZERO                                            
081203         IF OHUV-FLKLAR = 'N'                                             
081303           MOVE INF-ORDER-EJ-AVSLUT TO    MED-IDMFSINF                    
081403           CALL WMEDKONV            USING MED-WMEDAREA                    
081503           MOVE MED-MFSINF          TO    MOD-TEMFSINF                    
081603         END-IF                                                           
081703         IF OHUV-FLORDSPE = JA AND OHUV-IDSYSTEM NOT = 'W216'             
081803           IF OHUV-IDSYSTEM = 'SOFT'                                      
081903             MOVE SOFT-MED (SPRAK-IX)   TO MOD-INTERN                     
082003           ELSE                                                           
082103             MOVE INTERN-MED (SPRAK-IX) TO MOD-INTERN                     
082203           END-IF                                                         
082303         ELSE                                                             
082403           MOVE MFS-RENSA-FAELT         TO MOD-INTERN                     
082503         END-IF                                                           
082603         IF MSG-SIGNON-USERID(1:2) = 'A0' OR 'TI' OR 'PC'                 
082703           MOVE OHUV-IDSYSTEM           TO MOD-IDSYSTEM                   
082803         ELSE                                                             
082903           MOVE MFS-RENSA-FAELT         TO MOD-IDSYSTEM                   
083003         END-IF                                                           
084003         MOVE    OHUV-IDORDER TO W-ODEL-IDORDER-MIN                       
084103                                 W-ODEL-IDORDER-MAX                       
084203         PERFORM DAB-REDIGERA-ORDERHUVUDET                                
084303         IF ODEL-IDDC-EXP NOT = SPACE                                     
084403*          *Bounce order                                                  
084503           PERFORM IMS-GNP-ORQI12                                         
084603         ELSE                                                             
084703           PERFORM IMS-GNP-ORQI12-KVAL                                    
084803         END-IF                                                           
084903         IF SEGMENT-FINNS                                                 
085003           PERFORM DAC-REDIGERA-ARBETSTABELLEN                            
085103         ELSE                                                             
085203           PERFORM DAD-ARBETSTABELL-SAKNAS                                
085303         END-IF                                                           
085403       ELSE                                                               
085503         MOVE INF-ORDERHEAD-MISSING TO MED-IDMFSINF                       
085603         MOVE NEJ                   TO ALLT-SW                            
085703       END-IF                                                             
085803     END-IF                                                               
085903     .                                                                    
086003     EJECT                                                                
086103 DAA-KOLLA-ORQI12-SEGMENT SECTION.                                        
086203                                                                          
086303     IF SEGMENT-SAKNAS                                                    
086403       MOVE INF-ORDERINFO-BORTTAGEN TO    MED-IDMFSINF                    
086503     ELSE                                                                 
086603       MOVE INF-ORDER-ANNULLED      TO    MED-IDMFSINF                    
086703     END-IF                                                               
086803     MOVE NEJ                       TO    ALLT-SW                         
086903     .                                                                    
087003     EJECT                                                                
087103 DAB-REDIGERA-ORDERHUVUDET SECTION.                                       
087203                                                                          
087303     MOVE OHUV-KDORDKL         TO MOD-KDORDKL                             
087403     MOVE OHUV-KDFAKTYP        TO MOD-KDFAKTYP                            
087503     MOVE OHUV-BEGMT-RAD1      TO MOD-BEGMT-RAD1                          
087603     MOVE OHUV-BEGMT-RAD2      TO MOD-BEGMT-RAD2                          
087703     MOVE OHUV-ADGMT-GATA      TO MOD-ADGMT-GATA                          
087803     MOVE OHUV-ADGMT-PADR      TO MOD-ADGMT-PADR                          
087903     MOVE OHUV-ADGMT-LAND      TO MOD-ADGMT-LAND                          
088003                                                                          
088103     IF OHUV-IDKONTO > 0                                                  
088203       MOVE OHUV-IDKONTO       TO MOD-IDKONTO                             
088303       IF ENGLISH-TEXT                                                    
088403         MOVE 'Account'        TO MOD-KONTO                               
088503       ELSE                                                               
088603         MOVE 'Konto  '        TO MOD-KONTO                               
088703       END-IF                                                             
088803     END-IF                                                               
088903     IF OHUV-IDANALYS > 0                                                 
089003       MOVE OHUV-IDANALYS      TO MOD-IDANALYS                            
089103       IF ENGLISH-TEXT                                                    
089203         MOVE 'Int.Order'      TO MOD-ANALYS                              
089303       ELSE                                                               
089403         MOVE 'Analys nr'      TO MOD-ANALYS                              
089503       END-IF                                                             
089603     END-IF                                                               
089703     IF OHUV-IDKST > 0                                                    
089803       MOVE OHUV-IDKST         TO MOD-IDKST                               
089903       IF ENGLISH-TEXT                                                    
090003         MOVE 'Cc'             TO MOD-KST                                 
090103       ELSE                                                               
090203         MOVE 'Ks'             TO MOD-KST                                 
090303       END-IF                                                             
090403     END-IF                                                               
090503     .                                                                    
090603     EJECT                                                                
090703 DAC-REDIGERA-ARBETSTABELLEN SECTION.                                     
090803                                                                          
090903     MOVE   ARB-KDFRAKT            TO  MOD-KDFRAKT                        
091003     MOVE   ARB-TIHHMM             TO  WS-TID-N                           
091103     MOVE   WS-TID-X(2:2)          TO  WS-TID-HH                          
091203     MOVE   WS-TID-X(4:2)          TO  WS-TID-MM                          
091303     MOVE   '.'                    TO  WS-TID-PKT                         
091403     MOVE   WS-TID-RED-X           TO  MOD-TIHHMM                         
091503     MOVE   ARB-DATRPAVD (3:6)     TO  MOD-TIAAMMDD                       
091603     MOVE   ARB-IDTRP              TO  MOD-IDTRP                          
091703                                                                          
091803     IF (ARB-KDTRPKAT = 'B' OR 'C') AND ARB-TIRFS = +0                    
091903       PERFORM IMS-GU-GMTA-WDB201                                         
092003       IF SEGMENT-FINNS                                                   
092103         MOVE GMT-IDPARTNR            TO W-WDB1-IDPARTNR                  
092203         MOVE GMT-IDFTG               TO W-WDB1-IDFTG                     
092303         PERFORM IMS-GU-BETC-WDB101                                       
092403         IF SEGMENT-FINNS                                                 
092503           AND W-WDB1-IDPARTNR NOT = SPACE                                
092603           IF BET-KDKREDSP = '1'                                          
092703             MOVE SPAERR-MED (SPRAK-IX) TO MOD-INTERN                     
092803           ELSE                                                           
092903             MOVE MFS-RENSA-FAELT       TO MOD-INTERN                     
093003           END-IF                                                         
093103         ELSE                                                             
093203           MOVE MFS-RENSA-FAELT       TO MOD-INTERN                       
093303         END-IF                                                           
093403       END-IF                                                             
093503     END-IF                                                               
093603     .                                                                    
093703     EJECT                                                                
093803 DAD-ARBETSTABELL-SAKNAS SECTION.                                         
093903                                                                          
094003     PERFORM IMS-GNP-ORQI12-FIRST                                         
094103                                                                          
094203     IF SEGMENT-FINNS                                                     
094303       MOVE INF-LINES-MISSING-CL TO    MED-IDMFSINF                       
094403       CALL WMEDKONV             USING MED-WMEDAREA                       
094503       MOVE MED-MFSINF           TO    MOD-TEMFSINF                       
094603     ELSE                                                                 
094703       MOVE INF-ORDER-MISSING    TO    MED-IDMFSINF                       
094803       MOVE NEJ                  TO    ALLT-SW                            
094903     END-IF                                                               
095003     .                                                                    
095103     EJECT                                                                
095203 DB-KOLLA-BEHOERIGHET SECTION.                                            
095303                                                                          
095403     MOVE MSG-SIGNON-USERID TO    SEC-IDUSER                              
095503     MOVE '4501'            TO    SEC-IDTRANS                             
095603     MOVE MSGI-IDDISTR      TO    SEC-IDKEY                               
095703                                                                          
095803     CALL WSECURIT          USING SEC-IDUSER                              
095903                                  SEC-IDTRANS                             
096003                                  SEC-IDKEY                               
096103                                  SEC-KDSVAR                              
096203                                                                          
096303     IF SEC-KDSVAR = OBEHORIG                                             
096403       MOVE ERR-OBEHORIG TO MED-IDMFSFEL                                  
096503       MOVE NEJ          TO ALLT-SW                                       
096603     ELSE                                                                 
096703        CONTINUE                                                          
096803     END-IF                                                               
096903     .                                                                    
097003     EJECT                                                                
097103 F-REDIGERA-BILDEN SECTION.                                               
097203                                                                          
097303     MOVE +1 TO INDX                                                      
097403     PERFORM IMS-GU-ORQA01                                                
097503     IF SEGMENT-FINNS                                                     
097603       MOVE ODEL-IDDC     TO ODEL-IDDC-SPAR                               
097703       MOVE ODEL-IDPRODNR TO ODEL-IDPRODNR-SPAR                           
097803       MOVE ODEL-IDLEVNR  TO ODEL-IDLEVNR-SPAR                            
097903       PERFORM FAA-REDIGERA-ORDERDELEN                                    
098003                                                                          
098103       MOVE ODEL-IDPRODNR     TO MSGI-IDPRODNR                            
098203       MOVE '001'             TO MSGI-KDCALL                              
098303       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
098403       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
098503                                                                          
098603       ADD +1 TO INDX                                                     
098703       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
098803         IF SEGMENT-FINNS                                                 
098903           IF INDX          NOT > MAX-INDX            AND                 
099003             (ODEL-IDDC     NOT = ODEL-IDDC-SPAR)     OR                  
099103             (ODEL-IDPRODNR NOT = ODEL-IDPRODNR-SPAR) OR                  
099203             (ODEL-IDLEVNR  NOT = ODEL-IDLEVNR-SPAR )                     
099303             PERFORM FAA-REDIGERA-ORDERDELEN                              
099403             ADD     +1 TO INDX                                           
099503           END-IF                                                         
099603           IF ODEL-IDDC = DCS-IDDC                                        
099703             IF ODEL-KDODELSTA = 'R'                                      
099803               ADD ODEL-KVRADER  TO KVRADER-RAKNARE-WS                    
099903               ADD ODEL-SUORDV   TO SUORDV-RAKNARE-WS                     
100003               ADD ODEL-SUORDV-LOC TO SUORDV-LOC-RAKNARE-WS               
100103               ADD ODEL-SUORDV-LOCPREL                                    
100203                                 TO SUORDV-LOCPREL-RAKNARE-WS             
100303               ADD ODEL-VKORDNTO TO VKORDNTO-RAKNARE-WS                   
100403               ADD ODEL-VLORDNTO TO VLORDNTO-RAKNARE-WS                   
100503             END-IF                                                       
100603           ELSE                                                           
100703             ADD ODEL-KVRADER  TO KVRADER-RAKNARE-ALT                     
100803             ADD ODEL-SUORDV   TO SUORDV-RAKNARE-ALT                      
100903             ADD ODEL-SUORDV-LOC TO SUORDV-LOC-RAKNARE-ALT                
101003             ADD ODEL-SUORDV-LOCPREL                                      
101103                                 TO SUORDV-LOCPREL-RAKNARE-ALT            
101203             ADD ODEL-VKORDNTO TO VKORDNTO-RAKNARE-ALT                    
101303             ADD ODEL-VLORDNTO TO VLORDNTO-RAKNARE-ALT                    
101403           END-IF                                                         
101503           IF ODEL-KDODELSTA NOT = 'R'       AND                          
101603              (ODEL-IDDC         =  DCS-IDDC OR                           
101703               ODEL-IDDC-EXP     =  DCS-IDDC) AND                         
101803              ODEL-IDPRODNR  NOT =  VORD-IDPRODNR-SPAR                    
101903             MOVE ODEL-IDPRODNR TO W-VORD-IDPRODNR                        
102003             PERFORM IMS-GU-WDE601                                        
103003             IF SEGMENT-FINNS                                             
103103               IF VORD-IDDC = DCS-IDDC OR VORD-IDDC-EXP = DCS-IDDC        
103203                 IF VORD-IDDC-EXP = SPACE OR                              
103303                    VORD-IDDC-EXP = WC-CDC-SE                             
103403*                   *NORMAL ORDER OR BOUNCE AT DC 11                      
103503                    PERFORM FAD-HAMTA-SUORDV                              
103603                 END-IF                                                   
103703                 IF VORD-IDDC-EXP NOT = SPACE AND                         
103803                    VORD-IDDC-EXP NOT = WC-CDC-SE                         
103903*                   *BOUNCE VOR                                           
104003                    PERFORM FAE-HAMTA-SUORDV-VOR                          
105003                 END-IF                                                   
105103                 ADD  VORD-KVORDRAD   TO KVRADER-RAKNARE-WS               
105203                 ADD  VORD-SUORDV-LOC TO SUORDV-LOC-RAKNARE-WS            
105303                 ADD  VORD-SUORDV-LOCPREL                                 
105403                                      TO SUORDV-LOCPREL-RAKNARE-WS        
105503                 ADD  VORD-VKORDNTO   TO VKORDNTO-RAKNARE-WS              
105603                 ADD  VORD-VLORDNTO   TO VLORDNTO-RAKNARE-WS              
105703                 PERFORM FAC-REDIGERA-KOLLIREGISTER                       
105803                 MOVE   VORD-IDPRODNR TO ODEL-IDPRODNR-SPAR               
105903                                         VORD-IDPRODNR-SPAR               
106003               END-IF                                                     
106103             END-IF                                                       
106203           END-IF                                                         
106303           PERFORM IMS-GN-ORQA01                                          
106403         END-IF                                                           
106503       END-PERFORM                                                        
106504                                                                          
106603       IF KVRADER-RAKNARE-WS > +0                                         
106703         PERFORM S01-REDIGERA-RAKNARE                                     
106803       ELSE                                                               
106903         MOVE    JA TO ARB-SW                                             
107003         PERFORM FAB-LAES-ORQI11-OCH-ORQI21                               
107103         IF KVRADER-RAKNARE-WS  = +0 AND                                  
107203            KVRADER-RAKNARE-ALT = +0                                      
107303           MOVE INF-ORDERLINES-MISSING TO MED-IDMFSINF                    
107403           MOVE NEJ                    TO ARB-SW                          
107503         END-IF                                                           
107603         IF KVRADER-RAKNARE-WS      = +0 AND                              
107703            KVRADER-RAKNARE-ALT NOT = +0                                  
107803           MOVE INF-LINES-MISSING-CL TO MED-IDMFSINF                      
107903           MOVE NEJ                  TO ARB-SW                            
108003         END-IF                                                           
108103       END-IF                                                             
108203     ELSE                                                                 
108303       PERFORM FAB-LAES-ORQI11-OCH-ORQI21                                 
108403       IF KVRADER-RAKNARE-WS  = +0 AND                                    
108503          KVRADER-RAKNARE-ALT = +0                                        
108603         MOVE INF-ORDERLINES-MISSING TO MED-IDMFSINF                      
108703         MOVE NEJ                    TO ARB-SW                            
108803       END-IF                                                             
108903       IF KVRADER-RAKNARE-WS      = +0 AND                                
109003          KVRADER-RAKNARE-ALT NOT = +0                                    
109103         MOVE INF-LINES-MISSING-CL TO MED-IDMFSINF                        
109203         MOVE NEJ                  TO ARB-SW                              
109303       END-IF                                                             
109403     END-IF                                                               
109503     IF ARB-OK                  AND                                       
109603        KVRADER-RAKNARE-WS > +0                                           
109703       PERFORM S01-REDIGERA-RAKNARE                                       
109803*    ELSE                                                                 
109903*      IF ARB-FEL                                                         
110003*        CALL WMEDKONV   USING MED-WMEDAREA                               
110103*        MOVE MED-MFSINF TO    MOD-TEMFSINF                               
110203*      END-IF                                                             
110303     END-IF                                                               
110403     .                                                                    
110503     EJECT                                                                
110603 FAA-REDIGERA-ORDERDELEN SECTION.                                         
110703                                                                          
110803     MOVE      ODEL-IDDC     TO MOD-IDDC-RAD     (INDX)                   
110903                                ODEL-IDDC-SPAR                            
111003                                                                          
111103     MOVE      ODEL-IDDC-EXP TO MOD-IDDC-EXP                              
111203     MOVE      ODEL-IDPRODNR TO MOD-IDPRODNR-RAD (INDX)                   
111303                                ODEL-IDPRODNR-SPAR                        
111403     IF ODEL-IDLEVNR NOT = SPACE                                          
111503       MOVE    ODEL-IDLEVNR  TO MOD-IDLEVNR-RAD  (INDX)                   
111603                                ODEL-IDLEVNR-SPAR                         
111703     ELSE                                                                 
111803       MOVE    SPACE         TO ODEL-IDLEVNR-SPAR                         
111903       MOVE MFS-RENSA-FAELT  TO MOD-IDLEVNR-RAD  (INDX)                   
112003     END-IF                                                               
112103     IF DDGS-DCS-IDDC NOT = ODEL-IDDC                                     
112203        MOVE ODEL-IDDC TO W-IDDC-B6-DDGS                                  
112303        PERFORM IMS-GU-WDB601-DDGS                                        
112403     END-IF                                                               
112503     IF DDGS-DCS-DDC                                                      
112603       MOVE ODEL-IDDC        TO W-DIRL-IDDC                               
112703       MOVE ODEL-IDLEVNR     TO W-DIRL-IDLEVNR                            
112803       PERFORM IMS-GNP-ORQI11-UNIK                                        
112903       IF SEGMENT-FINNS                                                   
113003         MOVE DIRL-TISKEPPN-DDC TO MOD-TISKEPPN-DDC (INDX)                
113103       END-IF                                                             
113203     ELSE                                                                 
113303       MOVE MFS-RENSA-FAELT     TO MOD-TISKEPPN-DDC (INDX)                
113403     END-IF                                                               
113503     IF ODEL-IDDC = DCS-IDDC                                              
113603       IF ODEL-IDDC-EXP NOT = SPACE                                       
113703          IF ODEL-IDDC-EXP = WC-CDC-SE                                    
113803*      *BOUNCE ORDER REFILL                                               
113903             MOVE ODEL-KDVALISO TO WS-KDVALISO                            
114003             MOVE WS-TEDDI      TO MOD-TEDDI                              
114103          ELSE                                                            
114203*      *BOUNCE ORDER VOR                                                  
114303             MOVE WS-SEK        TO WS-KDVALISO                            
114403             MOVE WS-TEDDI      TO MOD-TEDDI                              
114503          END-IF                                                          
114603       ELSE                                                               
114703          MOVE ODEL-KDVALISO    TO WS-KDVALISO                            
114803          MOVE WS-TEDDI         TO MOD-TEDDI                              
114903       END-IF                                                             
115003     END-IF                                                               
115103     .                                                                    
115203     EJECT                                                                
115303 FAB-LAES-ORQI11-OCH-ORQI21 SECTION.                                      
115403                                                                          
115503     PERFORM FABA-LAES-ORQI11                                             
115603                                                                          
115703     IF ARB-TIRFS    = ZERO AND                                           
115803        ARB-DATRPAVD = ZERO AND                                           
115903        ARB-TIHHMM   = ZERO                                               
116003       MOVE INF-TRANSPORT-EJ-BOKAD TO MED-IDMFSINF                        
116103       CALL WMEDKONV USING MED-WMEDAREA                                   
116203       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
116303     END-IF                                                               
116403                                                                          
116508     MOVE ARB-IDDC TO W-ARB-IDDC                                          
116509                                                                          
116603     IF ARB-IDDC = DCS-IDDC                                               
116706       PERFORM IMS-GNP-ORQI21                                             
116803                                                                          
116903       PERFORM UNTIL SEGMENT-SAKNAS                                       
117003                                                                          
118005          ADD LOR-KVRADER        TO KVRADER-RAKNARE-WS                    
119005          ADD LOR-SUORDV         TO SUORDV-RAKNARE-WS                     
130005          ADD LOR-SUORDV-LOC     TO SUORDV-LOC-RAKNARE-WS                 
150005          ADD LOR-SUORDV-LOCPREL TO SUORDV-LOCPREL-RAKNARE-WS             
170005          ADD LOR-VKORDNTO       TO VKORDNTO-RAKNARE-WS                   
190005          ADD LOR-VLORDNTO       TO VLORDNTO-RAKNARE-WS                   
200005                                                                          
210003          PERFORM IMS-GNP-ORQI21                                          
220003                                                                          
230003       END-PERFORM                                                        
240003                                                                          
250003     ELSE                                                                 
260006       PERFORM IMS-GNP-ORQI21                                             
270003                                                                          
280003       PERFORM UNTIL SEGMENT-SAKNAS                                       
290003                                                                          
300005         ADD LOR-KVRADER        TO KVRADER-RAKNARE-ALT                    
320005         ADD LOR-SUORDV         TO SUORDV-RAKNARE-ALT                     
340005         ADD LOR-SUORDV-LOC     TO SUORDV-LOC-RAKNARE-ALT                 
360005         ADD LOR-SUORDV-LOCPREL TO SUORDV-LOCPREL-RAKNARE-ALT             
380005         ADD LOR-VKORDNTO       TO VKORDNTO-RAKNARE-ALT                   
400005         ADD LOR-VLORDNTO       TO VLORDNTO-RAKNARE-ALT                   
410005                                                                          
420003         PERFORM IMS-GNP-ORQI21                                           
430003                                                                          
440003       END-PERFORM                                                        
450003                                                                          
450103     END-IF                                                               
450203     .                                                                    
450303     EJECT                                                                
450403 FABA-LAES-ORQI11 SECTION.                                                
450503                                                                          
450603     PERFORM IMS-GNP-ORQI11-FIRST                                         
450703     IF SEGMENT-FINNS                                                     
450803       PERFORM UNTIL SEGMENT-SAKNAS                                       
450903         ADD DIRL-KVRADER  TO KVRADER-RAKNARE-WS                          
451003         ADD DIRL-SUORDV   TO SUORDV-RAKNARE-WS                           
451103         ADD DIRL-SUORDV-LOC  TO SUORDV-LOC-RAKNARE-WS                    
451203         ADD DIRL-SUORDV-LOCPREL  TO SUORDV-LOCPREL-RAKNARE-WS            
451303         ADD DIRL-VKORDNTO TO VKORDNTO-RAKNARE-WS                         
451403         ADD DIRL-VLORDNTO TO VLORDNTO-RAKNARE-WS                         
451503         PERFORM IMS-GNP-ORQI11                                           
451603       END-PERFORM                                                        
451703     END-IF                                                               
451803     .                                                                    
451903     EJECT                                                                
452003 FAC-REDIGERA-KOLLIREGISTER SECTION.                                      
452103                                                                          
452203     ADD VORD-KVKOLLI-FAKT    TO    KVKOLLI-FAKT-RAKNARE                  
452303     ADD VORD-KVKOLLI-LAST    TO    KVKOLLI-LAST-RAKNARE                  
452403     ADD VORD-KVKOLPAC        TO    KVKOLPAC-RAKNARE                      
452503     ADD VORD-KVORDRAD-PACK   TO    KVORDRAD-PACK-RAKNARE                 
452603     ADD VORD-VKORDBTO        TO    VKORDBTO-RAKNARE                      
452703     ADD VORD-VLORDBTO        TO    VLORDBTO-RAKNARE                      
452803     IF VORD-KVORDRAD = VORD-KVORDRAD-PACK AND                            
452903        VORD-KVKOLLI  = VORD-KVKOLPAC                                     
453003       CONTINUE                                                           
453103     ELSE                                                                 
453203       MOVE INF-ORDER-EJ-KLAR TO    MED-IDMFSINF                          
453303       CALL WMEDKONV          USING MED-WMEDAREA                          
453403       MOVE MED-MFSINF        TO    MOD-TEMFSINF                          
453503     END-IF                                                               
453603     .                                                                    
453703     EJECT                                                                
453803 FAD-HAMTA-SUORDV           SECTION.                                      
453903                                                                          
454003     IF VORD-IDDC = DCS-IDDC AND                                          
454103        VORD-SUORDV-EXP > 0                                               
454203*       *SUORDV-EXP BASED ON PRAVCOST.                                    
454303        ADD VORD-SUORDV-EXP TO SUORDV-EXP-RAKNARE-WS                      
454403        MOVE VORD-KDVALISO-EXP TO WS-KDVALISO                             
454503        MOVE WS-TEDDI     TO MOD-TEDDI                                    
454603     ELSE                                                                 
454703        IF VORD-IDDC-EXP NOT = SPACE                                      
454803*         *Export order, show only if order invoiced                      
454903          IF VORD-KVKOLLI-FAKT > 0                                        
455003            ADD  VORD-SUORDV  TO SUORDV-RAKNARE-WS                        
455103            MOVE VORD-KDVALISO TO WS-KDVALISO                             
455203            MOVE WS-TEDDI      TO MOD-TEDDI                               
455303          ELSE                                                            
455403            MOVE SPACE         TO WS-KDVALISO                             
455503            MOVE WS-TEDDI      TO MOD-TEDDI                               
455603          END-IF                                                          
455703        ELSE                                                              
455803          ADD  VORD-SUORDV   TO SUORDV-RAKNARE-WS                         
455903          MOVE VORD-KDVALISO TO WS-KDVALISO                               
456003          MOVE WS-TEDDI      TO MOD-TEDDI                                 
456103        END-IF                                                            
456203     END-IF                                                               
456303     .                                                                    
456403     EJECT                                                                
456503 FAE-HAMTA-SUORDV-VOR       SECTION.                                      
456603                                                                          
456703     IF VORD-IDDC = DCS-IDDC                                              
456803*       *SUORDV BASED ON PRARTNTO (DC=11)                                 
456903        ADD VORD-SUORDV     TO SUORDV-EXP-RAKNARE-WS                      
457003        MOVE WS-SEK         TO WS-KDVALISO                                
457103        MOVE WS-TEDDI       TO MOD-TEDDI                                  
457203     ELSE                                                                 
457303        IF VORD-IDDC-EXP = DCS-IDDC                                       
457403*          *Bounce-dc wants to see the order                              
457503          IF VORD-KVKOLLI-FAKT > 0                                        
457603*          *Show only if order invoiced                                   
457703            ADD  VORD-SUORDV-EXP   TO SUORDV-RAKNARE-WS                   
457803            MOVE VORD-KDVALISO-EXP TO WS-KDVALISO                         
457903            MOVE WS-TEDDI          TO MOD-TEDDI                           
458003          ELSE                                                            
458103            MOVE SPACE         TO WS-KDVALISO                             
458203            MOVE WS-TEDDI      TO MOD-TEDDI                               
458303          END-IF                                                          
458403        END-IF                                                            
458503     END-IF                                                               
458603     .                                                                    
458703     EJECT                                                                
458803 S01-REDIGERA-RAKNARE SECTION.                                            
458903                                                                          
459003     IF KVRADER-RAKNARE-WS    > ZERO                                      
459103       MOVE KVRADER-RAKNARE-WS     TO MOD-KVRADER                         
459203     ELSE                                                                 
459303       MOVE MFS-RENSA-FAELT        TO MOD-KVRADER                         
459403     END-IF                                                               
459503                                                                          
459603     IF DIST79-DEALER-PRICE OR                                            
459803        DIST79-ECOM-PRICE                                                 
459903        PERFORM S01A-RED-RAKNARE-DIST79                                   
460003                                                                          
460103     ELSE                                                                 
460203       IF SUORDV-RAKNARE-WS     > ZERO                                    
460303         IF SEC-KDSVAR = 2 OR 6                                           
460403           MOVE MFS-RENSA-FAELT      TO MOD-SUORDV                        
460503         ELSE                                                             
460603           MOVE SUORDV-RAKNARE-WS    TO MOD-SUORDV                        
460703         END-IF                                                           
460803       ELSE                                                               
460903         IF SUORDV-EXP-RAKNARE-WS > ZERO                                  
461003           IF SEC-KDSVAR = 2 OR 6                                         
461103             MOVE MFS-RENSA-FAELT       TO MOD-SUORDV                     
461203           ELSE                                                           
461303             MOVE SUORDV-EXP-RAKNARE-WS TO MOD-SUORDV                     
461403           END-IF                                                         
461503         ELSE                                                             
461603           MOVE MFS-RENSA-FAELT         TO MOD-SUORDV                     
461703         END-IF                                                           
461803       END-IF                                                             
461903       MOVE ' '                      TO MOD-TEASTRIX                      
462003     END-IF                                                               
462103                                                                          
462203     IF VKORDNTO-RAKNARE-WS   > ZERO                                      
462303       IF WS-KDMATT = 'U'                                                 
462403         COMPUTE VKORDNTO-RAKNARE-WS =                                    
462503                 VKORDNTO-RAKNARE-WS * CONV-KG-TO-LB                      
462603                                                                          
462703         MOVE VKORDNTO-RAKNARE-WS  TO MOD-VKORDNTO                        
462803       ELSE                                                               
462903         MOVE VKORDNTO-RAKNARE-WS  TO MOD-VKORDNTO                        
463003       END-IF                                                             
463103     ELSE                                                                 
463203       MOVE MFS-RENSA-FAELT        TO MOD-VKORDNTO                        
463303     END-IF                                                               
463403                                                                          
463503     IF VLORDNTO-RAKNARE-WS   > ZERO                                      
463603       IF WS-KDMATT = 'U'                                                 
463703         COMPUTE VLORDNTO-RAKNARE-WS =                                    
463803                 VLORDNTO-RAKNARE-WS * CONV-M3-TO-FT3                     
463903                                                                          
464003         MOVE VLORDNTO-RAKNARE-WS  TO MOD-VLORDNTO                        
464103       ELSE                                                               
464203         MOVE VLORDNTO-RAKNARE-WS  TO MOD-VLORDNTO                        
464303       END-IF                                                             
464403     ELSE                                                                 
464503       MOVE MFS-RENSA-FAELT        TO MOD-VLORDNTO                        
464603     END-IF                                                               
464703                                                                          
464803     IF KVKOLLI-FAKT-RAKNARE  > ZERO                                      
464903       MOVE KVKOLLI-FAKT-RAKNARE   TO MOD-KVKOLLI-FAKT                    
465003     ELSE                                                                 
465103       MOVE MFS-RENSA-FAELT        TO MOD-KVKOLLI-FAKT                    
465203     END-IF                                                               
465303                                                                          
465403     IF KVKOLLI-LAST-RAKNARE  > ZERO                                      
465503       MOVE KVKOLLI-LAST-RAKNARE   TO MOD-KVKOLLI-LAST                    
465603     ELSE                                                                 
465703       MOVE MFS-RENSA-FAELT        TO MOD-KVKOLLI-LAST                    
465803     END-IF                                                               
465903                                                                          
466003     IF KVKOLPAC-RAKNARE      > ZERO                                      
466103       MOVE KVKOLPAC-RAKNARE       TO MOD-KVKOLPAC                        
466203     ELSE                                                                 
466303       MOVE MFS-RENSA-FAELT        TO MOD-KVKOLPAC                        
466403     END-IF                                                               
466503                                                                          
466603     IF KVORDRAD-PACK-RAKNARE > ZERO                                      
466703       MOVE KVORDRAD-PACK-RAKNARE  TO MOD-KVORDRAD-PACK                   
466803     ELSE                                                                 
466903       MOVE MFS-RENSA-FAELT        TO MOD-KVORDRAD-PACK                   
467003     END-IF                                                               
467103                                                                          
467203     IF VKORDBTO-RAKNARE      > ZERO                                      
467303       IF WS-KDMATT = 'U'                                                 
467403         COMPUTE VKORDBTO-RAKNARE =                                       
467503                 VKORDBTO-RAKNARE * CONV-KG-TO-LB                         
467603                                                                          
467703         MOVE VKORDBTO-RAKNARE     TO MOD-VKORDBTO                        
467803       ELSE                                                               
467903         MOVE VKORDBTO-RAKNARE     TO MOD-VKORDBTO                        
468003       END-IF                                                             
468103     ELSE                                                                 
468203       MOVE MFS-RENSA-FAELT        TO MOD-VKORDBTO                        
468303     END-IF                                                               
468403                                                                          
468503     IF VLORDBTO-RAKNARE      > ZERO                                      
468603       IF WS-KDMATT = 'U'                                                 
468703         COMPUTE VLORDBTO-RAKNARE =                                       
468803                 VLORDBTO-RAKNARE * CONV-M3-TO-FT3                        
468903                                                                          
469003         MOVE VLORDBTO-RAKNARE     TO MOD-VLORDBTO                        
469103       ELSE                                                               
469203         MOVE VLORDBTO-RAKNARE     TO MOD-VLORDBTO                        
469303       END-IF                                                             
469403     ELSE                                                                 
469503       MOVE MFS-RENSA-FAELT        TO MOD-VLORDBTO                        
469603     END-IF                                                               
469703     .                                                                    
469803     EJECT                                                                
469903 S01A-RED-RAKNARE-DIST79 SECTION.                                         
470003                                                                          
470103                                                                          
470203     COMPUTE HELP-SUMMA = SUORDV-LOC-RAKNARE-WS +                         
470303                          SUORDV-LOCPREL-RAKNARE-WS                       
470403     IF HELP-SUMMA     > ZERO                                             
470503       IF SEC-KDSVAR = 2 OR 6                                             
470603         MOVE MFS-RENSA-FAELT      TO MOD-SUORDV                          
470703       ELSE                                                               
470803         MOVE HELP-SUMMA           TO MOD-SUORDV                          
470903       END-IF                                                             
471003     ELSE                                                                 
471103       MOVE MFS-RENSA-FAELT        TO MOD-SUORDV                          
471203     END-IF                                                               
471303     IF SUORDV-LOCPREL-RAKNARE-WS = +0                                    
471403       MOVE ' '                    TO MOD-TEASTRIX                        
471503     ELSE                                                                 
471603       MOVE '*'                    TO MOD-TEASTRIX                        
471703     END-IF                                                               
471803     .                                                                    
471903     EJECT                                                                
472003                                                                          
472103 MFS-RENSA-FAELT-UT SECTION.                                              
472203                                                                          
472303*    --- ALLA UTDATA-FÄLT                                                 
472403     MOVE MFS-RENSA-FAELT TO MOD-KDORDKL                                  
472503                             MOD-KDFRAKT                                  
472603                             MOD-KDFAKTYP                                 
472703                             MOD-BEGMT-RAD1                               
472803                             MOD-BEGMT-RAD2                               
472903                             MOD-ADGMT-GATA                               
473003                             MOD-ADGMT-PADR                               
473103                             MOD-ADGMT-LAND                               
473203                             MOD-TIAAMMDD                                 
473303                             MOD-TIHHMM                                   
473403                             MOD-IDTRP                                    
473503                             MOD-INTERN                                   
473603                             MOD-IDSYSTEM                                 
473703                             MOD-IDORDER                                  
473803                             MOD-KVRADER                                  
473903                             MOD-KVKOLPAC                                 
474003                             MOD-VKORDNTO                                 
474103                             MOD-VKORDBTO                                 
474203                             MOD-KVORDRAD-PACK                            
474303                             MOD-KVKOLLI-LAST                             
474403                             MOD-VLORDNTO                                 
474503                             MOD-VLORDBTO                                 
474603                             MOD-SUORDV                                   
474703                             MOD-KVKOLLI-FAKT                             
474803                             MOD-KONTO                                    
474903                             MOD-IDKONTO                                  
475003                             MOD-IDANALYS                                 
475103                             MOD-IDKST                                    
475203                                                                          
475303     MOVE +1 TO INDX                                                      
475403     PERFORM UNTIL INDX > MAX-INDX                                        
475503       MOVE MFS-RENSA-FAELT TO MOD-IDDC-RAD     (INDX)                    
475603       MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-RAD (INDX)                    
475703       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-RAD  (INDX)                    
475803       MOVE MFS-RENSA-FAELT TO MOD-TISKEPPN-DDC (INDX)                    
475903       ADD  +1              TO INDX                                       
476003     END-PERFORM                                                          
476103     .                                                                    
476203     EJECT                                                                
476303* --- IMS SEKTIONER ---                                                   
476403     SKIP3                                                                
476503 IMS-GET-MSG SECTION.                                                     
476603                                                                          
476703     MOVE '  QC' TO GODK-STATUSKODER                                      
476803     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
476903     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
477003     PERFORM IMS-STATUSKONTROLL                                           
477103     .                                                                    
477203     SKIP3                                                                
477303 IMS-INSERT-MSG SECTION.                                                  
477403                                                                          
477503     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
477603       MOVE '0' TO MFS-KDHUVOMR                                           
477703     END-IF                                                               
477803     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
477903     MOVE SPACE TO GODK-STATUSKODER                                       
478003     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
478103     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
478203     PERFORM IMS-STATUSKONTROLL                                           
478303     .                                                                    
478403     EJECT                                                                
478503 IMS-GU-ORQI01-M-Q2CSEQ SECTION.                                          
478603                                                                          
478703     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
478803          DELIMITED BY SIZE INTO SSA1                                     
478903     MOVE '  GE' TO GODK-STATUSKODER                                      
479003     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
479103     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
479203     PERFORM IMS-STATUSKONTROLL                                           
479303     .                                                                    
479403     EJECT                                                                
479503 IMS-GNP-ORQI11-UNIK SECTION.                                             
479603                                                                          
479703     STRING 'WLORQI11*F(WDQ211KY =' W-WDQ211KY-X ')'                      
479803          DELIMITED BY SIZE INTO SSA1                                     
479903     MOVE '  GE' TO GODK-STATUSKODER                                      
480003     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-DIRL SSA1                
480103     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
480203     PERFORM IMS-STATUSKONTROLL                                           
480303     .                                                                    
480403     SKIP2                                                                
480503 IMS-GNP-ORQI11-FIRST SECTION.                                            
480603                                                                          
480703     MOVE 'WLORQI11*F' TO SSA1                                            
480803     MOVE '  GE' TO GODK-STATUSKODER                                      
480903     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-DIRL SSA1                
481003     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
481103     PERFORM IMS-STATUSKONTROLL                                           
481203     .                                                                    
481303     SKIP2                                                                
481403 IMS-GNP-ORQI11 SECTION.                                                  
481503                                                                          
481603     MOVE 'WLORQI11' TO SSA1                                              
481703     MOVE '  GE' TO GODK-STATUSKODER                                      
481803     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-DIRL SSA1                
481903     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
482003     PERFORM IMS-STATUSKONTROLL                                           
482103     .                                                                    
482203     EJECT                                                                
482303 IMS-GNP-ORQI12-KVAL SECTION.                                             
482403                                                                          
482503     STRING 'WLORQI12(IDDC     =' W-IDDC-X    ')'                         
482603          DELIMITED BY SIZE INTO SSA1                                     
482703     MOVE '  GE' TO GODK-STATUSKODER                                      
482803     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
482903     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
483003     PERFORM IMS-STATUSKONTROLL                                           
483103     .                                                                    
483203     SKIP2                                                                
483303 IMS-GNP-ORQI12-FIRST SECTION.                                            
483403                                                                          
483503     MOVE   'WLORQI12*F'      TO SSA1                                     
483603     MOVE '  GE' TO GODK-STATUSKODER                                      
483703     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
483803     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
483903     PERFORM IMS-STATUSKONTROLL                                           
484003     .                                                                    
484103     SKIP2                                                                
484203 IMS-GNP-ORQI12 SECTION.                                                  
484303                                                                          
484403     MOVE 'WLORQI12' TO SSA1                                              
484503     MOVE '  GE' TO GODK-STATUSKODER                                      
484603     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
484703     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
484803     PERFORM IMS-STATUSKONTROLL                                           
484903     .                                                                    
485003     EJECT                                                                
486003 IMS-GNP-ORQI21 SECTION.                                                  
486103                                                                          
486207     STRING 'WLORQI12(IDDC     =' W-IDDC-X    ')'                         
486307          DELIMITED BY SIZE INTO SSA1                                     
486407     MOVE   'WLORQI21'        TO SSA2                                     
486503     MOVE '  GE' TO GODK-STATUSKODER                                      
486607     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-LOR SSA1 SSA2            
486703     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
486803     PERFORM IMS-STATUSKONTROLL                                           
486903     .                                                                    
487003     EJECT                                                                
487103 IMS-GU-WDE601 SECTION.                                                   
487203                                                                          
487303     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
487403          DELIMITED BY SIZE INTO SSA1                                     
487503     MOVE '  GE' TO GODK-STATUSKODER                                      
487603     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA-VORD SSA1                 
487703     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
487803     PERFORM IMS-STATUSKONTROLL                                           
487903     .                                                                    
488003     EJECT                                                                
488103 IMS-GU-ORQA01 SECTION.                                                   
488203                                                                          
488303     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
488403                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
488503          DELIMITED BY SIZE INTO SSA1                                     
488603     MOVE '  GBGE' TO GODK-STATUSKODER                                    
488703     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
488803     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
488903     PERFORM IMS-STATUSKONTROLL                                           
489003     .                                                                    
489103     EJECT                                                                
489203 IMS-GN-ORQA01 SECTION.                                                   
489303                                                                          
489403     STRING 'WLORQA01(WDQ301KY >' W-WDQ301KY-MIN-X                        
489503                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
489603          DELIMITED BY SIZE INTO SSA1                                     
489703     MOVE '  GBGE' TO GODK-STATUSKODER                                    
489803     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA-ODEL SSA1                 
489903     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
490003     PERFORM IMS-STATUSKONTROLL                                           
490103     .                                                                    
490203     EJECT                                                                
490303 IMS-GU-ORQA01-M-Q3DSEQ SECTION.                                          
490403                                                                          
490503     STRING 'WLORQA01(WDQ3DSEQ>=' W-WDQ3DSEQ-MIN-X                        
490603                    '&WDQ3DSEQ<=' W-WDQ3DSEQ-MAX-X ')'                    
490703          DELIMITED BY SIZE INTO SSA1                                     
490803     MOVE '  GBGE' TO GODK-STATUSKODER                                    
490903     CALL CBLTDLI USING GU ORQAD-PCB DLI-IO-AREA-ODEL SSA1                
491003     MOVE ORQAD-STATUS-CODE TO STATUS-WS                                  
491103     PERFORM IMS-STATUSKONTROLL                                           
491203     .                                                                    
491303     EJECT                                                                
491403 IMS-GU-GMTA-WDB201 SECTION.                                              
491503                                                                          
491603     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
491703          DELIMITED BY SIZE INTO SSA1                                     
491803     MOVE '  GE'              TO GODK-STATUSKODER                         
491903                                                                          
492003     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-GMTA SSA1                 
492103     MOVE GMTA-STATUS-CODE    TO STATUS-WS                                
492203     PERFORM IMS-STATUSKONTROLL                                           
492303     .                                                                    
492403     EJECT                                                                
492503 IMS-GU-BETC-WDB101 SECTION.                                              
492603                                                                          
492703     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
492803          DELIMITED BY SIZE INTO SSA1                                     
492903     MOVE '  GE'              TO GODK-STATUSKODER                         
493003                                                                          
493103     CALL CBLTDLI USING GU BETC-PCB DLI-IO-AREA-BETC SSA1                 
493203     MOVE BETC-STATUS-CODE    TO STATUS-WS                                
493303     PERFORM IMS-STATUSKONTROLL                                           
493403     .                                                                    
493503     EJECT                                                                
493603 IMS-GU-WDB601    SECTION.                                                
493703     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
493803          DELIMITED BY SIZE INTO SSA1                                     
493903     MOVE '  GE' TO GODK-STATUSKODER                                      
494003     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
494103     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
494203     PERFORM IMS-STATUSKONTROLL                                           
494303     .                                                                    
494403     EJECT                                                                
494503                                                                          
494603 IMS-GU-WDB601-DDGS   SECTION.                                            
494703     STRING 'WDB601  (IDDC     =' W-IDDC-B6-DDGS-X ')'                    
494803          DELIMITED BY SIZE INTO SSA1                                     
494903     MOVE '  GE' TO GODK-STATUSKODER                                      
495003     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-DDGS SSA1            
495103     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
495203     PERFORM IMS-STATUSKONTROLL                                           
495303     .                                                                    
495403     EJECT                                                                
495503                                                                          
495603 IMS-STATUSKONTROLL SECTION.                                              
495703                                                                          
495803     SET STATUS-IX TO 1                                                   
495903     SEARCH GODK-STATUS                                                   
496003       AT END CALL FELLOG                                                 
496103       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
496203     END-SEARCH                                                           
497003     .                                                                    
