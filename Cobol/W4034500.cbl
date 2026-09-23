000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W4034500.                                                
000301 AUTHOR.         LENA BROMANDER.                                          
000401 DATE-WRITTEN.   16/06/30.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701                                                                          
000801                                                                          
000901*    FUNKTION:                                                            
001001*      1.  ÖPPNAR NYTT SAMLINGSKOLLI                                      
001114*      2.  VISAR SAMLINGSKOLLI UTIFRÅN TRANSPORT,VO OCH DC SAMT           
001214*          FR O M  2019 ÄVEN KDFRAKT-SKTRP. KDFRAKT-SKTRP ÄR INTE         
001215*          OBLIGATORISK, UTELÄMNAR MAN DEN FÅR DEN VÄRDET NOLL OCH        
001216*          KRÄVER INTE ATT INGÅENDE KOLLIN HAR SAMMA KDFRAKT.             
001314*      3.  LÄGGER TILL/TAR BORT KOLLI I SAMLINGSKOLLIT                    
001414*      4.  AVSLUTAR OCH SKRIVER KOLLIFLAGGA                               
001514*          OCH FÖLJSEDEL TILL SAMLINGSKOLLIT                              
001614*      5.  ÅTERÖPPNAR SAMLINGSKOLLI                                       
001714*      6.  SKRIVER KOLLIFLAGGA OCH/ELLER FÖLJESEDEL                       
001814*          FÖR STÄNGT SAMLINGSKOLLI.                                      
001914*                                                                         
002014*                                                                         
002114*        PROGRAMMET LÄSER/UPPD WDE7                                       
002214*        PROGRAMMET LÄSER/UPPD WDE6                                       
002314*        PROGRAMMET LÄSER      WDE4                                       
002414*        PROGRAMMET LÄSER      WDK5                                       
002514*                                                                         
002614*    INDATA.                                                              
002714*        TRANSAKTION: W4T345                                              
002814*        MID:         W4I34501                                            
002914                                                                          
003014*        OBS  -  OBS  -  OBS  -  OBS   !!!!                               
003114                                                                          
003215*        INMATNING AV TRP-VO-FK, DISTR-KUND-ORDER-KOLLI KAN ÄVEN          
003314*        SKE MHA SCANNER. SCANNERN ÄR BEROENDE AV POSITIONEN PÅ           
003414*        FÄLTEN PÅ BILDEN. SÅ ÄNDRAS POSITIONEN PÅ                        
003514*        DESSA FÄLT I MID MÅSTE SCANNER PROGRAMMERAS OM.                  
003614*                                                                         
003714*    UTDATA.                                                              
003814*        MOD:         W4O34501                                            
003914                                                                          
004014     SKIP3                                                                
004114 ENVIRONMENT DIVISION.                                                    
004214                                                                          
004314 DATA DIVISION.                                                           
004414     EJECT                                                                
004514 WORKING-STORAGE SECTION.                                                 
004614 77  IDPGM                       PIC X(08)   VALUE 'W4034500'.            
004714                                                                          
004814 77  CURRENT-SECTION             PIC X(24)   VALUE SPACE.                 
004914 77  CURRENT-IMS-SECTION         PIC X(24)   VALUE SPACE.                 
005014                                                                          
005114                                                                          
005214*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005314 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005414                                                                          
005514 77  JA                          PIC X       VALUE 'J'.                   
005614 77  YES                         PIC X       VALUE 'Y'.                   
005714 77  NEJ                         PIC X       VALUE 'N'.                   
005814 77  RAETT                       PIC X       VALUE 'R'.                   
005914                                                                          
006014*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006114                                                                          
006214                                                                          
006314 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006414     88  NYCKLAR-OK                          VALUE 'J'.                   
006514     88  NYCKLAR-FEL                         VALUE 'N'.                   
006614                                                                          
006714 77  NOT-NUMERIC-SW              PIC X       VALUE 'J'.                   
006814     88  NUMERIC-OK                          VALUE 'J'.                   
006914     88  NOT-NUMERIC                         VALUE 'N'.                   
007014                                                                          
007114 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007214     88  INDATA-OK                           VALUE 'J'.                   
007314     88  INDATA-FEL                          VALUE 'N'.                   
007414                                                                          
007514 77  KOMB-TRP-VO-OK-SW           PIC X       VALUE 'J'.                   
007614     88  KOMB-TRP-VO-OK                      VALUE 'J'.                   
007714     88  KOMB-TRP-VO-NOK                     VALUE 'N'.                   
007814                                                                          
007914                                                                          
008014 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008114     88  EGEN-MID                            VALUE '4345'.                
008214     88  GODK-MID                            VALUE '4341' '4342'          
008314                                                   '4343' '4344'          
008414                                                   '4345' '4346'          
008514                                                   '4347' '4348'          
008614                                                   '4349'.                
008714     88  HELP-MID                            VALUE '0551'.                
008814*    --- ARBETSFÄLT                                                       
008914 01  ARBETSFALT.                                                          
009014     03  WS-IDDISTR              PIC S9(5)   VALUE ZERO.                  
009114     03  WS-IDKUNDNR             PIC S9(7)   VALUE ZERO.                  
009214     03  WS-IDORDNR7             PIC 9(7)    VALUE ZERO.                  
009314     03  WS-IDTRPTNR             PIC S9(3)   VALUE ZERO.                  
009414     03  WS-KDFRAKT-SKTRP        PIC S9(3)   VALUE ZERO.                  
009514     03  WS-DATUM-TID.                                                    
009614       05 WS-DATUM               PIC X(8)    VALUE SPACE.                 
009714       05 WS-KLOCKAN             PIC 9(10)   VALUE ZERO.                  
009814     03  WS-SKLI-IDKOLLI-SAMP-X.                                          
009914       05  WS-SKLI-IDVO          PIC X(2)    VALUE ZERO.                  
010014       05  WS-SKLI-LOPNR         PIC X(3)    VALUE ZERO.                  
010114     03  WS-SKLI-IDKOLLI-SAMP    PIC 9(5)    VALUE ZERO .                 
010214     03  WS-FLFARLIG             PIC X       VALUE 'N'.                   
010314     03  IX                      PIC 9(3)    VALUE 1.                     
010414     03  WS-KDFRAKT-1            PIC S9(3)   VALUE ZERO.                  
010514     03  WS-KDFRAKT-2            PIC S9(3)   VALUE ZERO.                  
010614     03  WS-KDFRAKT-3            PIC S9(3)   VALUE ZERO.                  
010714     03  WS-ATGARD               PIC X       VALUE SPACE.                 
010814     03  WS-KDKOLLI              PIC X(8)    VALUE SPACE.                 
010914     03  WS-IDKOLLI              PIC 9(5)    VALUE ZERO .                 
011014     03  WS-IDPRODNR             PIC S9(7)   VALUE ZERO COMP-3.           
011114     03  WS-DIKOLLIH-SAMP        PIC 9(3)    VALUE ZERO.                  
011214     03  W-EMB-DIKOLLIH          PIC S9(3)   VALUE ZERO.                  
011314     03  WS-IDDC-IN              PIC X(2)    VALUE SPACE.                 
011414                                                                          
011514*    --- WS-BEHKOD FÖR ÅTGÄRD = BLANK                                     
011614 77  WS-BEHKOD                   PIC X       VALUE SPACE.                 
011714     88  AVSLUTA-SK                          VALUE '1'.                   
011814     88  VISA-SK-STATUS-O-R                  VALUE '2'.                   
011914     88  ADD-DEL-TILL-VALT-SK                VALUE '3'.                   
012014     88  ADD-DEL-TILL-OPPET-SK               VALUE '4'.                   
012114     88  VISA-VALT-SK                        VALUE '5'.                   
012214     88  UPPD-ATGARD                         VALUE '6'.                   
012314     88  PRINTA-FLAGGA-ELLER-FS              VALUE '7'.                   
012414                                                                          
012514                                                                          
012614 77  TRAFF-SW                PIC  X(1)        VALUE 'N'.                  
012714     88  TRAFF                                VALUE 'J'.                  
012814 77  TRAFF-IDDC-SW           PIC  X(1)        VALUE 'N'.                  
012914     88  TRAFF-IDDC                           VALUE 'J'.                  
013014 77  TRAFF-IDKOLLI-SW        PIC  X(1)        VALUE 'N'.                  
013114     88  TRAFF-IDKOLLI                        VALUE 'J'.                  
013214     88  EJ-TRAFF-IDKOLLI                     VALUE 'N'.                  
013314 77  TRAFF-OPPET-SK-SW       PIC  X(1)        VALUE 'N'.                  
013414     88  TRAFF-OPPET-SK                       VALUE 'J'.                  
013514     88  EJ-TRAFF-OPPET-SK                    VALUE 'N'.                  
013614 77  TRAFF-STATUS-O-R-SW     PIC  X(1)        VALUE 'N'.                  
013714     88  TRAFF-STATUS-O-R                     VALUE 'J'.                  
013814     88  EJ-TRAFF-STATUS-O-R                  VALUE 'N'.                  
013914 77  SOEKNYCKEL-SK-SW        PIC  X(1)        VALUE ' '.                  
014014     88  SOEKNYCKEL-SK                        VALUE 'J'.                  
014114     88  SOEKNYCKEL-TRP-VO                    VALUE 'N'.                  
014214     88  SOEKNYCKEL-EJ-VALD                   VALUE ' '.                  
014314                                                                          
014414 77  STATUS-UPD                  PIC X       VALUE 'N'.                   
014514     88  UPD-GJORD                           VALUE 'J'.                   
014614     88  UPD-EJGJORD                         VALUE 'N'.                   
014714                                                                          
014814                                                                          
014914 01  WS-SUMMERINGAR.                                                      
015014     03  WS-ANTAL-KOLLIN-I-SAMP  PIC 9(4)    VALUE ZERO.                  
015114     03  WS-VKORDNTO-SAMP        PIC S9(9)V9(1) VALUE ZERO.               
015214     03  WS-VKORDBTO-SAMP        PIC S9(9)V9(1) VALUE ZERO.               
015314                                                                          
015414                                                                          
015514 01  WS-IDPRTLST-ADR.                                                     
015614     03 WS-SYSTDEL-ADR           PIC X(1).                                
015714     03 WS-LISTTYP-ADR           PIC X(2).                                
015814     03 WS-DC-ADR                PIC X(2).                                
015914     03 WS-KDPRTVAL-ADR          PIC X(2).                                
016014                                                                          
016114 01  WS-IDPRTLST-FS.                                                      
016214     03 WS-SYSTDEL-FS            PIC X(1).                                
016314     03 WS-LISTTYP-FS            PIC X(2).                                
016414     03 WS-DC-FS                 PIC X(2).                                
016514     03 WS-KDPRTVAL-FS           PIC X(2).                                
016614                                                                          
016714 01  WS-E611-SK-KOPPL            PIC X       VALUE 'N'.                   
016814                                                                          
016914*      --- VALID IDDC CODES                                               
017014*                                                                         
017114*01    -COPY WWDC99                                                       
017214       EJECT                                                              
017314                                                                          
017414     EJECT                                                                
017514 01  FILLER                  PIC X(16) VALUE 'KONSTANTER'.                
017614 01  KONSTANTER.                                                          
017714     03  OPENED              PIC  X(1)        VALUE 'O'.                  
017814     03  REOPENED            PIC  X(1)        VALUE 'R'.                  
017914     03  CLOSED              PIC  X(1)        VALUE 'C'.                  
018014     03  SHIPPED             PIC  X(1)        VALUE 'S'.                  
018114     03  KLI-PACK            PIC S9(1) COMP-3 VALUE +1.                   
018214     03  W-FEL-795           PIC  X(3)        VALUE '795'.                
018314     03  W-FEL-758           PIC  X(3)        VALUE '758'.                
018414                                                                          
018514                                                                          
018614     EJECT                                                                
018714*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
018814 01  GENERELLA-SUBPROGRAM.                                                
018914     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019014     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
019114     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019214     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019314     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
019414     EJECT                                                                
019514*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
019614*01 -COPY WMEDAREA                                                        
019714     SKIP3                                                                
019814 01  MESSAGE-CODES.                                                       
019914     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
020014     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
020114     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
020214     03  ERR-MISSING-ORDER       PIC X(3)    VALUE '054'.                 
020314     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
020414     03  ERR-WRONG-IDDISTR       PIC X(3)    VALUE '747'.                 
020514     03  ERR-WRONG-TRANSPORT     PIC X(3)    VALUE '252'.                 
020614     03  ERR-CASE-EXISTS         PIC X(3)    VALUE '251'.                 
020714     03  ERR-WRONG-CASE-MIXEDPACK PIC X(3)   VALUE '250'.                 
020814     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
020914     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
021014     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
021114     03  INF-PRINT-STARTED       PIC X(3)    VALUE '202'.                 
021214     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
021314     03  INF-THIS-IS-THE-LAST-PAG PIC X(3)   VALUE '106'.                 
021414     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
021514                                                                          
021614                                                                          
021714*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
021814*                                                                         
021914 01  FILLER                      PIC X(16)  VALUE 'LÄNKAREOR'.            
022014*                                                                         
022114 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
022214*01 -COPY WMSGINIT                                                        
022314 01  FILLER                      PIC X(16)   VALUE 'W006PRT  '.           
022414*01 -COPY W006PRT                                                         
022514*                                                                         
022614     EJECT                                                                
022714*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
022814*                                                                         
022914 01  SPAR-AREA.                                                           
023014     03  SPAR-IDTRANS           PIC X(4)    VALUE '4345'.                 
023114     EJECT                                                                
023214*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
023314*                                                                         
023414 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023514     SKIP3                                                                
023614*01  MID -COPY W4I34501                                                   
023714     EJECT                                                                
023814 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023914     SKIP3                                                                
024014*01  -COPY WMSGAREA                                                       
024114     EJECT                                                                
024214     03  MOD REDEFINES MSG-AREA.                                          
024314*      05  -COPY W4O34501                                                 
024414     EJECT                                                                
024514 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024614     SKIP3                                                                
024714*01  -COPY WMFSAREA                                                       
024814     EJECT                                                                
024914*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025014*                                                                         
025114 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025214     SKIP3                                                                
025314 01  NYCKLAR-TILL-DLI.                                                    
025414     03  W-WDE701KY.                                                      
025514         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO COMP-3.           
025614         05  W-IDVO              PIC 9(2)    VALUE ZERO.                  
025714         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
025814         05  W-KDFRAKT-SKTRP     PIC S9(3)   VALUE ZERO COMP-3.           
025914     03  W-IDPRODNR-X.                                                    
026014         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
026114     03  W-IDKOLLI-SAMP-X.                                                
026214         05  W-IDKOLLI-SAMP      PIC S9(5)   VALUE ZERO COMP-3.           
026314     03  W-WDE721KY.                                                      
026414         05 W-721-IDDISTR        PIC S9(5)   VALUE ZERO  COMP-3.          
026514         05 W-721-IDKUNDNR       PIC S9(7)   VALUE ZERO  COMP-3.          
026614         05 W-721-IDORDNR7       PIC 9(7)    VALUE ZERO.                  
026714         05 W-721-IDKOLLI        PIC S9(5)   VALUE ZERO COMP-3.           
026814     03  W-WDE7ASEQ-X.                                                    
026914         05 W-7A1-IDDC           PIC X(2)    VALUE SPACE.                 
027014         05 W-7A1-IDKOLLI-SAMP   PIC S9(5)   VALUE ZERO  COMP-3.          
027502                                                                          
027602     03  W-IDKOLLI-X.                                                     
027702         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
027802                                                                          
027902     03  W-WDE4ASEQ-X.                                                    
028002         05 W-4A1-IDDISTR        PIC S9(5)   VALUE ZERO  COMP-3.          
028102         05 W-4A1-IDKUNDNR       PIC S9(7)   VALUE ZERO  COMP-3.          
028202         05 W-4A1-IDKUNDRF.                                               
028302           07 W-4A1-IDORDNR      PIC X(5).                                
028402           07 FILLER             PIC X(5).                                
028502                                                                          
028602     03  W-KDKOLLI-K5-X.                                                  
028702         05  W-KDKOLLI-K5        PIC X(8)    VALUE SPACE.                 
028802                                                                          
028902*      SERIENR FÖR ATT SKAPA NYTT SAMLINGSKOLLINR                         
029002   03  W-WDGXKEY-4539-X.                                                  
029102     05  W-IDHTYP-4539           PIC X(4)    VALUE '4539'.                
029202     05  W-IDVO-4539             PIC 9(2)    VALUE ZERO.                  
029302     05  W-IDDC-4539             PIC X(2)    VALUE SPACE.                 
029402     05  FILLER                  PIC X(22)   VALUE LOW-VALUE.             
029502*                                                                         
029602   03  W-WDGXKEY-4540-X.                                                  
029702     05  W-KDSEGKEY              PIC X(1)    VALUE '1'.                   
029802                                                                          
029902**** MID FÖR ANROP PRINTPROGRAM W4034P00 (KOLLIFLAGGA)                    
030002   01  FILLER                  PIC X(16) VALUE 'MID W4I34P01AREA'.        
030102   01  434P-MSG-IO-AREA.                                                  
030202                                                                          
030302       03  434P-KVLL             PIC S9(4)   COMP SYNC.                   
030402       03  434P-Z1               PIC X.                                   
030502       03  434P-Z2               PIC X.                                   
030602       03  434P-TRANSKOD         PIC X(8)    VALUE 'W4T34PX '.            
030702       03  434P-IDTRANS          PIC X(4)    VALUE '4345'.                
030802       03  434P-KDMFSFOR         PIC X.                                   
030902*      03  MID -COPY W4I34P01  -PRE 434P-.                                
031002     SKIP2                                                                
031102                                                                          
031202**** MID FÖR ANROP PRINTPROGRAM W4034S00 (FÖLJESEDEL)                     
031302   01  FILLER                  PIC X(16) VALUE 'MID W4I34S01AREA'.        
031402   01  434S-MSG-IO-AREA.                                                  
031502                                                                          
031602       03  434S-KVLL             PIC S9(4)   COMP SYNC.                   
031702       03  434S-Z1               PIC X.                                   
031802       03  434S-Z2               PIC X.                                   
031902       03  434S-TRANSKOD         PIC X(8)    VALUE 'W4T34SX '.            
032002       03  434S-IDTRANS          PIC X(4)    VALUE '4345'.                
032102       03  434S-KDMFSFOR         PIC X.                                   
032202*      03  MID -COPY W4I34S01  -PRE 434S-.                                
032302                                                                          
032402     SKIP2                                                                
032502*    --- STATUS KODER FRÅN IMS                                            
032602 01  STATUS-WS                   PIC XX.                                  
032702     88  SEGMENT-FINNS                       VALUE '  '.                  
032802     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
032902     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
033002     88  BASEN-SLUT                          VALUE 'GB'.                  
033102     88  DUPL-KEY-SEQ-INDEX                  VALUE 'NI'.                  
033202     SKIP2                                                                
033302 01  GODK-STATUSKODER.                                                    
033402     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033502     SKIP3                                                                
033602 01  SSA1                        PIC X(64).                               
033702 01  SSA2                        PIC X(64).                               
033802 01  SSA3                        PIC X(64).                               
033902     EJECT                                                                
034002*    --- IMS FUNKTIONSKODER                                               
034102*01  -COPY W0003                                                          
034202     EJECT                                                                
034302*    ---  DLI INPUT-OUTPUT AREA                                           
034402                                                                          
034502 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE701'.                      
034602 01  DLI-IO-WDE701.                                                       
034702*    03  -COPY WDE701                                                     
034802 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE711'.                      
034902 01  DLI-IO-WDE711.                                                       
035002*    03  -COPY WDE711                                                     
035102 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE721'.                      
035202 01  DLI-IO-WDE721.                                                       
035302*    03  -COPY WDE721                                                     
035402 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
035502 01  DLI-IO-WDE601.                                                       
035602*    03  -COPY WDE601                                                     
035702     EJECT                                                                
035802 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
035902 01  DLI-IO-WDE611.                                                       
036002*    03  -COPY WDE611                                                     
036102 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
036202 01  DLI-IO-WDE401.                                                       
036302*    03  -COPY WDE401                                                     
036402     EJECT                                                                
036502 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK501'.                      
036602 01  DLI-IO-WDK501.                                                       
036702*    03  -COPY WDK501                                                     
036802 01  FILLER         PIC X(16)   VALUE 'DLI-IO-4540'.                      
036902     SKIP3                                                                
037002 01  DLI-IO-4540.                                                         
037102*    03  -COPY WDGX4540                                                   
037202                                                                          
037302     EJECT                                                                
037402 LINKAGE SECTION.                                                         
037502*01  -COPY W0009   -PRE MSG-                                              
037602                                                                          
037702*01  -COPY W0009   -PRE ALT434P-                                          
037802                                                                          
037902*01  -COPY W0009   -PRE ALT434S-                                          
038002                                                                          
038102*01  -COPY W0008   -PRE WDP7-                                             
038202     05  FILLER                  PIC X.                                   
038302                                                                          
038402*01  -COPY W0008  -PRE WDE7-                                              
038502     05  FILLER                  PIC X.                                   
038602                                                                          
038702*01  -COPY W0008  -PRE WDE7A-                                             
038802     05  FILLER                  PIC X.                                   
038902                                                                          
039002*01  -COPY W0008  -PRE WDE6-                                              
039102     05  FILLER                  PIC X.                                   
039202                                                                          
039302*01  -COPY W0008  -PRE WDE4-                                              
039402     05  FILLER                  PIC X.                                   
039502                                                                          
039602*01  -COPY W0008  -PRE WDK5-                                              
039702     05  FILLER                  PIC X.                                   
039802                                                                          
039902*01  -COPY W0008  -PRE 4539-                                              
040002     05  FILLER                  PIC X.                                   
040102                                                                          
040202     EJECT                                                                
040302 PROCEDURE DIVISION  USING MSG-PCB ALT434P-PCB                            
040402                           ALT434S-PCB                                    
040502                           WDP7-PCB WDE7-PCB                              
040602                           WDE7A-PCB WDE6-PCB                             
040702                           WDE4-PCB WDK5-PCB                              
040802                           4539-PCB.                                      
040902 MAIN SECTION.                                                            
041002     ENTRY 'DLITCBL' USING MSG-PCB ALT434P-PCB                            
041102                           ALT434S-PCB                                    
041202                           WDP7-PCB WDE7-PCB                              
041302                           WDE7A-PCB WDE6-PCB                             
041402                           WDE4-PCB WDK5-PCB                              
041502                           4539-PCB.                                      
041602                                                                          
041702                                                                          
041802     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-DATUM                        
041902     MOVE FUNCTION CURRENT-DATE (9:8)  TO WS-KLOCKAN                      
042002                                                                          
042102     PERFORM IMS-GET-MSG                                                  
042202*--- DISPLAY 'START W40345 ' WS-DATUM ' ' WS-KLOCKAN                      
042302                                                                          
042402     IF SEGMENT-FINNS                                                     
042502       PERFORM A-INIT                                                     
042602                                                                          
042702       PERFORM B-KOLLA-NYCKLAR                                            
042802       IF NYCKLAR-OK                                                      
042902         IF MFS-UPDATE                                                    
043002           PERFORM D-KOLLA-INPUT                                          
043102           IF INDATA-OK                                                   
043202             PERFORM E-UPPDATERA                                          
043302           END-IF                                                         
043402         ELSE                                                             
043502*    --- ENTER FRÅN SKÄRM (KAN VARA ISRT AV 701-SEGM)                     
043602           PERFORM G-SATT-BEHKOD                                          
043702         END-IF                                                           
043802         IF INDATA-OK                                                     
043902           PERFORM C-LAES-VISA-INFO                                       
044002         END-IF                                                           
044102       END-IF                                                             
044202                                                                          
044302*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
044402*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
044502       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O34501 + 4                      
044602                                                                          
044702       PERFORM IMS-INSERT-MSG                                             
044802     END-IF                                                               
044902                                                                          
045002     MOVE ZERO TO RETURN-CODE                                             
045102     GOBACK                                                               
045202     .                                                                    
045302     EJECT                                                                
045402 A-INIT SECTION.                                                          
045502                                                                          
045602     IF MSG-DUBBLA-TRANSKODER                                             
045702       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I34501                 
045802       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
045902       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
046002     ELSE                                                                 
046102       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I34501                  
046202       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
046302       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
046402     END-IF                                                               
046502                                                                          
046602     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
046702     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
046802     MOVE MFS-IDTRANS TO W-IDTRANS                                        
046902                                                                          
047002     MOVE LOW-VALUE TO MSG-AREA                                           
047102     MOVE 'W4O345N1' TO MFS-IDMOD                                         
047202     MOVE '4345' TO MOD-IDTRANS                                           
047302     MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                                 
047402                                                                          
047502*--- VI FLYTTAR SPACE TILL MOD-TEMFSFEL PGA FÄLTET ÄR                     
047602*--- INDIKATOR TILL SCANNERN OM DEN SKA GE FELPIP ELLER EJ                
047702     MOVE SPACE           TO MOD-TEMFSFEL                                 
047802                                                                          
047902                                                                          
048002     IF EGEN-MID OR HELP-MID                                              
048102       CONTINUE                                                           
048202     ELSE                                                                 
048302       MOVE SPACE TO MFS-KDTRTYP                                          
048402       MOVE '7' TO MFS-IDPFK                                              
048502     END-IF                                                               
048602                                                                          
048702     .                                                                    
048802     EJECT                                                                
048902 B-KOLLA-NYCKLAR SECTION.                                                 
049002                                                                          
049102     MOVE 'B-KOLLA-NYCKLAR    '    TO CURRENT-SECTION                     
049202                                                                          
049302     MOVE ALL '+'           TO MSGI-WMSGINIT                              
049402     MOVE '001'             TO MSGI-KDCALL                                
049502     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
049602     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
049702     MOVE '4345'            TO MSGI-IDTRANS                               
049802                                                                          
049902     IF EGEN-MID                                                          
050002         MOVE MID-KDPRTVAL-ADR    TO MSGI-KDPRTVAL-ADR                    
050102         MOVE MID-KDPRTVAL-FS     TO MSGI-KDPRTVAL-FS                     
050202     END-IF                                                               
050302     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
050402     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
050502                                                                          
050602*--- SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                   
050702     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
050802*LB  MOVE MSGI-IDDC       TO W-IDDC                                       
050902*LB                          WS-IDDC                                      
051002*LB                          W-7A1-IDDC                                   
051102     MOVE JA TO NYCKLAR-SW                                                
051202                                                                          
051302                                                                          
051402*--- KONTROLL AV NYCKELFÄLT                                               
051502     MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-IN                              
051602                             MOD-IDVO-IN                                  
051702                             MOD-IDKOLLI-SAMP-IN                          
051809                             MOD-KDFRAKT-SKTRP-IN                         
051902                                                                          
052002     IF MID-IDTRPTNR-IN   NOT = ALL '+' OR                                
052102        MID-IDVO-IN       NOT = ALL '+' OR                                
052202        MID-IDKOLLI-SAMP-IN NOT = ALL '+' OR                              
052309        MID-KDFRAKT-SKTRP-IN NOT = ALL '+'                                
052402       MOVE '7'         TO MFS-IDPFK                                      
052502       MOVE SPACE       TO MFS-KDTRTYP                                    
052602     END-IF                                                               
052702                                                                          
052802*--- VI HÄMTAR ENDAST FRÅN MID- EJ MSGI-                                  
052902                                                                          
053002*--- IDTRPTNR                                                             
053102                                                                          
053202     IF MID-IDTRPTNR-IN  = ALL '+'                                        
053302       IF MID-IDTRPTNR-UT NOT = ALL '+'                                   
053402         MOVE MID-IDTRPTNR-UT     TO MID-IDTRPTNR-IN                      
053502       END-IF                                                             
053602     ELSE                                                                 
053702       MOVE NEJ            TO SOEKNYCKEL-SK-SW                            
053802     END-IF                                                               
053902                                                                          
054002     IF MID-IDTRPTNR-IN NOT = ALL '+'                                     
054102       INSPECT MID-IDTRPTNR-IN REPLACING LEADING SPACE BY ZERO            
054202                                                                          
054302       IF MID-IDTRPTNR-IN NUMERIC                                         
054402          MOVE MID-IDTRPTNR-IN TO W-IDTRPTNR                              
054502       ELSE                                                               
054602         MOVE NEJ TO NYCKLAR-SW                                           
054702         MOVE NEJ TO NOT-NUMERIC-SW                                       
054802       END-IF                                                             
054902     END-IF                                                               
055002                                                                          
055102*--- IDVO                                                                 
055202                                                                          
055302     IF MID-IDVO-IN  = ALL '+'                                            
055402       IF  MID-IDVO-UT NOT = ALL '+'                                      
055502         MOVE MID-IDVO-UT     TO MID-IDVO-IN                              
055602       END-IF                                                             
055702     ELSE                                                                 
055802       MOVE NEJ            TO SOEKNYCKEL-SK-SW                            
055902     END-IF                                                               
056002                                                                          
056102     IF MID-IDVO-IN  NOT = ALL '+'                                        
056202       INSPECT MID-IDVO-IN   REPLACING LEADING SPACE BY ZERO              
056302                                                                          
056402       IF MID-IDVO-IN NUMERIC                                             
056502         MOVE MID-IDVO-IN   TO W-IDVO                                     
056602       ELSE                                                               
056702         MOVE NEJ TO NYCKLAR-SW                                           
056802         MOVE NEJ TO NOT-NUMERIC-SW                                       
056902       END-IF                                                             
057002     END-IF                                                               
057102                                                                          
057209*--- KDFRAKT-SKTRP                                                        
057302                                                                          
057409     IF MID-KDFRAKT-SKTRP-IN = ALL '+'                                    
057509       IF MID-KDFRAKT-SKTRP-UT NOT = ALL '+'                              
057609         MOVE MID-KDFRAKT-SKTRP-UT    TO MID-KDFRAKT-SKTRP-IN             
057702       END-IF                                                             
057802     ELSE                                                                 
057902       MOVE NEJ            TO SOEKNYCKEL-SK-SW                            
058002     END-IF                                                               
058102                                                                          
058209     IF MID-KDFRAKT-SKTRP-IN NOT = ALL '+'                                
058309       INSPECT MID-KDFRAKT-SKTRP-IN                                       
058409               REPLACING LEADING SPACE BY ZERO                            
058509                                                                          
058609       IF MID-KDFRAKT-SKTRP-IN NUMERIC                                    
058709          MOVE MID-KDFRAKT-SKTRP-IN TO W-KDFRAKT-SKTRP                    
058809       ELSE                                                               
058909         MOVE NEJ TO NYCKLAR-SW                                           
059009         MOVE NEJ TO NOT-NUMERIC-SW                                       
059109       END-IF                                                             
059209                                                                          
059317*---   OM TRP EL.VO IFYLLT MEN INTE FRAKTKOD, I SÅ FALL                   
059411*---   SÄTTER VI FRAKTKOD TILL NOLL                                       
059511     ELSE                                                                 
059611       IF SOEKNYCKEL-TRP-VO                                               
059711          MOVE ZERO        TO MID-KDFRAKT-SKTRP-IN                        
059811                              W-KDFRAKT-SKTRP                             
059911       END-IF                                                             
060111     END-IF                                                               
060211                                                                          
060311                                                                          
060411*--- IDKOLLI-SAMP                                                         
060511                                                                          
060611     IF MID-IDKOLLI-SAMP-IN  = ALL '+'                                    
060711       IF  MID-IDKOLLI-SAMP-UT NOT = ALL '+'                              
060811         MOVE MID-IDKOLLI-SAMP-UT     TO MID-IDKOLLI-SAMP-IN              
060911       END-IF                                                             
061011     ELSE                                                                 
061111       MOVE JA             TO SOEKNYCKEL-SK-SW                            
061211     END-IF                                                               
061311                                                                          
061411     IF MID-IDKOLLI-SAMP-IN  NOT = ALL '+'                                
061511       INSPECT MID-IDKOLLI-SAMP-IN REPLACING LEADING SPACE BY ZERO        
061611                                                                          
061711       IF MID-IDKOLLI-SAMP-IN NUMERIC                                     
061811         MOVE MID-IDKOLLI-SAMP-IN TO W-IDKOLLI-SAMP                       
061911                                    W-7A1-IDKOLLI-SAMP                    
062011       ELSE                                                               
062111         MOVE NEJ TO NYCKLAR-SW                                           
062211         MOVE NEJ TO NOT-NUMERIC-SW                                       
062311       END-IF                                                             
062411     END-IF                                                               
062511                                                                          
063001*--- IDDC                                                                 
064001                                                                          
065001     MOVE MSGI-IDDC           TO WS-IDDC                                  
066001                                                                          
067001     IF CDC-SE                                                            
068001        IF MID-IDDC-IN NOT = ALL '+'                                      
069001          MOVE MID-IDDC-IN      TO WS-IDDC                                
069101          IF CDC-SE OR DDC-SE                                             
069201             MOVE MID-IDDC-IN   TO WS-IDDC-IN                             
069301             MOVE '7'           TO MFS-IDPFK                              
069401             MOVE SPACE         TO MFS-KDTRTYP                            
069501          ELSE                                                            
069601             MOVE MSGI-IDDC     TO WS-IDDC-IN                             
069701          END-IF                                                          
069801        ELSE                                                              
069901          MOVE MID-IDDC-UT      TO WS-IDDC                                
070001          IF DDC-SE                                                       
070101             MOVE MID-IDDC-UT   TO WS-IDDC-IN                             
070201          ELSE                                                            
070301             MOVE MSGI-IDDC     TO WS-IDDC-IN                             
070401          END-IF                                                          
070501        END-IF                                                            
070601     ELSE                                                                 
070701        MOVE MSGI-IDDC          TO WS-IDDC-IN                             
070801     END-IF                                                               
070901                                                                          
071001     MOVE WS-IDDC-IN            TO W-IDDC                                 
071101                                   W-7A1-IDDC                             
071201                                   MOD-IDDC-UT                            
071301                                                                          
071401*--- IDDC + IDVO GILTIGT? / KDFRAKT GILTIGT?                              
071501                                                                          
071601     IF NYCKLAR-OK AND                                                    
071701       SOEKNYCKEL-TRP-VO                                                  
071801       PERFORM BD-KOLLA-IDVO                                              
072401     END-IF                                                               
072501                                                                          
072601     IF NYCKLAR-OK                                                        
072701       PERFORM BA-KOLLA-KOMBINATION                                       
072801     END-IF                                                               
072901                                                                          
073001     IF GODK-MID AND NYCKLAR-OK                                           
073101                                                                          
073200       IF SOEKNYCKEL-TRP-VO                                               
073301         MOVE MID-IDTRPTNR-IN     TO MOD-IDTRPTNR-UT                      
073401         INSPECT MOD-IDTRPTNR-UT  REPLACING LEADING ZERO BY SPACE         
073500                                                                          
073601         MOVE MID-IDVO-IN         TO MOD-IDVO-UT                          
073701         INSPECT MOD-IDVO-UT      REPLACING LEADING ZERO BY SPACE         
073800                                                                          
073909         MOVE MID-KDFRAKT-SKTRP-IN TO MOD-KDFRAKT-SKTRP-UT                
074009         INSPECT MOD-KDFRAKT-SKTRP-UT                                     
074101                                  REPLACING LEADING ZERO BY SPACE         
074201                                                                          
074301         MOVE MFS-RENSA-FAELT     TO MOD-IDKOLLI-SAMP-UT                  
074401       ELSE                                                               
074501         IF SOEKNYCKEL-SK                                                 
074601           MOVE MID-IDKOLLI-SAMP-IN TO MOD-IDKOLLI-SAMP-UT                
074701           INSPECT MOD-IDKOLLI-SAMP-UT                                    
074801                   REPLACING LEADING ZERO BY SPACE                        
074901           MOVE MFS-RENSA-FAELT   TO MOD-IDTRPTNR-UT                      
075001                                       MOD-IDVO-UT                        
075109                                       MOD-KDFRAKT-SKTRP-UT               
075201*LB                                    MOD-IDDC-UT                        
075301         ELSE                                                             
075401*---       INGEN SÖKNING VALD ALT HOPP FRÅN ANNAN BILD                    
075501           MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-UT                        
075601                                   MOD-IDVO-UT                            
075709                                   MOD-KDFRAKT-SKTRP-UT                   
075801                                   MOD-IDKOLLI-SAMP-UT                    
075901         END-IF                                                           
076001       END-IF                                                             
076101                                                                          
076201*LB    MOVE MSGI-IDDC            TO MOD-IDDC-UT                           
076301       MOVE MSGI-KDPRTVAL-ADR    TO MOD-KDPRTVAL-ADR                      
076401       MOVE MSGI-KDPRTVAL-FS     TO MOD-KDPRTVAL-FS                       
076501     ELSE                                                                 
076601       MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-UT                            
076701                               MOD-IDVO-UT                                
076801                               MOD-IDDC-UT                                
076909                               MOD-KDFRAKT-SKTRP-UT                       
077001                               MOD-IDKOLLI-SAMP-UT                        
077101     END-IF                                                               
077201                                                                          
077301     IF NYCKLAR-FEL                                                       
077401       IF NOT-NUMERIC                                                     
077501         MOVE 'NYCKLAR INTE NUMERISKT'    TO MOD-TEMFSFEL                 
077601       ELSE                                                               
077701         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
077801         CALL WMEDKONV USING MED-WMEDAREA                                 
077901         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
078001       END-IF                                                             
078101       PERFORM MFS-RENSA-FAELT-IN                                         
078201       PERFORM MFS-RENSA-FAELT-UT                                         
078301     END-IF                                                               
078401                                                                          
078516*--- VI BETRAKTAR KOLLIRADEN, SOM EN FÖRLÄNGNING AV NYCKELRADEN,          
078601*--- DÄRAV I B- SEKTION                                                   
078701                                                                          
078801     IF NYCKLAR-OK                                                        
078901       PERFORM BB-KOLLA-KOLLI                                             
079001       IF NYCKLAR-OK                                                      
079101         PERFORM BC-KOLLA-KOLLI-KOMB                                      
079201       END-IF                                                             
079301       IF NYCKLAR-FEL                                                     
079401         PERFORM MFS-ROER-EJ-FAELT-IN                                     
079501         PERFORM MFS-ROER-EJ-FAELT-UT                                     
079601         PERFORM MFS-LAES-IN-IGEN                                         
079701       END-IF                                                             
079801     END-IF                                                               
079901     .                                                                    
080001     EJECT                                                                
080101                                                                          
080201 BA-KOLLA-KOMBINATION SECTION.                                            
080301                                                                          
080401     MOVE 'BA-KOLLA-KOMBINATION'   TO CURRENT-SECTION                     
080501                                                                          
080609*--- IDTRPTNR + IDVO + KDFRAKT-SKTRP FÅR ENDAST FYLLAS I IHOP             
080717*--- VI KOLLAR ENDAST TRP-VO-FK OM DET INTE REDAN ÄR BESTÄMT              
080801*--- ATT DET ÄR SK VI SKA HA SOM NYCKEL                                   
080901*--- DVS   URSPRUNGLIG MID-IDKOLLI-SAMP-IN IFYLLD                         
081001                                                                          
081004                                                                          
081101     IF SOEKNYCKEL-SK                                                     
081201       CONTINUE                                                           
081301     ELSE                                                                 
081402       MOVE JA TO KOMB-TRP-VO-OK-SW                                       
081502                                                                          
081601       IF (MID-IDTRPTNR-IN NUMERIC AND                                    
081701         MID-IDTRPTNR-IN > ZERO AND                                       
081801         MID-IDVO-IN NOT NUMERIC)                                         
081901         OR                                                               
082001        (MID-IDTRPTNR-IN NUMERIC AND                                      
082101         MID-IDTRPTNR-IN > ZERO AND                                       
082201         MID-IDVO-IN NUMERIC AND                                          
082301         MID-IDVO-IN NOT > ZERO)                                          
082401         OR                                                               
082501        (MID-IDTRPTNR-IN NOT NUMERIC AND                                  
082601         MID-IDVO-IN NUMERIC AND                                          
082701         MID-IDVO-IN > ZERO)                                              
082801         OR                                                               
082901        (MID-IDTRPTNR-IN NUMERIC AND                                      
083001         MID-IDTRPTNR-IN NOT > ZERO AND                                   
083101         MID-IDVO-IN NUMERIC AND                                          
083201         MID-IDVO-IN > ZERO)                                              
083301                                                                          
083402         MOVE NEJ TO KOMB-TRP-VO-OK-SW                                    
083501         MOVE NEJ TO NYCKLAR-SW                                           
083601       END-IF                                                             
083702                                                                          
083802*---   OM BÅDE TRP+VO IFYLLDA ELLER BÅDE EJ-IFYLLDA                       
083902*---   KOLLA ATT FRAKTKOD ÄR MOTSVARADE, IFYLLD/EJ-IFYLLD                 
084002                                                                          
084102       IF KOMB-TRP-VO-OK                                                  
084103         IF (MID-IDTRPTNR-IN NUMERIC AND                                  
084104           MID-IDTRPTNR-IN > ZERO AND                                     
084105           MID-KDFRAKT-SKTRP-IN        NOT NUMERIC)                       
084108           MOVE NEJ TO NYCKLAR-SW                                         
084109         END-IF                                                           
084110                                                                          
084111         IF (MID-IDTRPTNR-IN NUMERIC AND                                  
084112           MID-IDTRPTNR-IN > ZERO AND                                     
084113           MID-KDFRAKT-SKTRP-IN        NUMERIC AND                        
084114           MID-KDFRAKT-SKTRP-IN        < ZERO)                            
084116           MOVE NEJ TO NYCKLAR-SW                                         
084117         END-IF                                                           
084120                                                                          
084130         IF (MID-IDTRPTNR-IN NOT NUMERIC AND                              
084140           MID-KDFRAKT-SKTRP-IN         NUMERIC AND                       
084150           MID-KDFRAKT-SKTRP-IN     NOT < ZERO)                           
084152           MOVE NEJ TO NYCKLAR-SW                                         
084153         END-IF                                                           
084160                                                                          
084170         IF (MID-IDTRPTNR-IN NUMERIC AND                                  
084180           MID-IDTRPTNR-IN NOT > ZERO AND                                 
084190           MID-KDFRAKT-SKTRP-IN         NUMERIC AND                       
084200           MID-KDFRAKT-SKTRP-IN         > ZERO)                           
084300                                                                          
084303           MOVE NEJ TO NYCKLAR-SW                                         
084304         END-IF                                                           
084305                                                                          
085802       END-IF                                                             
085902                                                                          
086002     END-IF                                                               
086102                                                                          
086202                                                                          
086302*--- EN AV IDKOLLI-SAMP ELLER IDTRPTNR + IDVO MÅSTE VARA IFYLLD           
086402*--- INGET IFYLLT I URSPRUNGLIGA MID-FÄLT-IN                              
086502                                                                          
086602     IF SOEKNYCKEL-EJ-VALD                                                
086702       IF ((MID-IDTRPTNR-IN NOT NUMERIC) OR                               
086802         (MID-IDTRPTNR-IN NUMERIC AND                                     
086902          MID-IDTRPTNR-IN NOT > ZERO))                                    
087002         AND                                                              
087102         ((MID-IDVO-IN NOT NUMERIC) OR                                    
087202         (MID-IDVO-IN NUMERIC AND                                         
087302          MID-IDVO-IN NOT > ZERO))                                        
087402         AND                                                              
087509         ((MID-KDFRAKT-SKTRP-IN NOT NUMERIC) OR                           
087609         (MID-KDFRAKT-SKTRP-IN NUMERIC AND                                
087709          MID-KDFRAKT-SKTRP-IN < ZERO))                                   
087802         AND                                                              
087902         ((MID-IDKOLLI-SAMP-IN NOT NUMERIC) OR                            
088002         (MID-IDKOLLI-SAMP-IN NUMERIC AND                                 
088102          MID-IDKOLLI-SAMP-IN NOT > ZERO))                                
088202         MOVE NEJ TO NYCKLAR-SW                                           
088302       END-IF                                                             
088402     END-IF                                                               
088502                                                                          
088602     IF NYCKLAR-OK AND                                                    
088702       SOEKNYCKEL-EJ-VALD                                                 
088802       IF MID-IDKOLLI-SAMP-IN NUMERIC AND                                 
088902         MID-IDKOLLI-SAMP-IN > ZERO                                       
089002         MOVE JA             TO SOEKNYCKEL-SK-SW                          
089102       ELSE                                                               
089202         MOVE NEJ            TO SOEKNYCKEL-SK-SW                          
089302       END-IF                                                             
089402     END-IF                                                               
089502     .                                                                    
089602     EJECT                                                                
089702 BB-KOLLA-KOLLI   SECTION.                                                
089802                                                                          
089902     IF EGEN-MID                                                          
090002       IF MID-IDDISTR                  NOT = ALL '+'                      
090102         MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-IDDISTR-ATTR                
090202         IF MID-IDDISTR NUMERIC AND                                       
090302           MID-IDDISTR > ZERO                                             
090402           MOVE MID-IDDISTR            TO W-4A1-IDDISTR                   
090610                                          WS-IDDISTR                      
090710                                          W-721-IDDISTR                   
090802         ELSE                                                             
090902           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDDISTR-ATTR                
091010           MOVE MID-IDDISTR            TO MOD-IDDISTR                     
091102           MOVE NEJ TO NYCKLAR-SW                                         
091202         END-IF                                                           
091302       ELSE                                                               
091402         MOVE MFS-RENSA-FAELT          TO MOD-IDDISTR                     
091502       END-IF                                                             
091602                                                                          
091702                                                                          
091802       IF MID-IDKUNDNR                 NOT = ALL '+'                      
091902         IF MID-IDKUNDNR NUMERIC AND                                      
092002           (MID-IDKUNDNR = ZERO OR                                        
092102            MID-IDKUNDNR > ZERO)                                          
092202           MOVE MID-IDKUNDNR           TO W-4A1-IDKUNDNR                  
092410                                          WS-IDKUNDNR                     
092510                                          W-721-IDKUNDNR                  
092602         ELSE                                                             
092710           MOVE MID-IDKUNDNR           TO MOD-IDKUNDNR                    
092802           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDKUNDNR-ATTR               
092902           MOVE NEJ TO NYCKLAR-SW                                         
093002         END-IF                                                           
093102       ELSE                                                               
093202         MOVE MFS-RENSA-FAELT          TO MOD-IDKUNDNR                    
093302       END-IF                                                             
093402                                                                          
093502       IF MID-IDORDNR7                 NOT = ALL '+'                      
093602         IF MID-IDORDNR7 NUMERIC AND                                      
093702           MID-IDORDNR7 > ZERO                                            
093802           MOVE SPACE                  TO W-4A1-IDKUNDRF                  
093902           MOVE MID-IDORDNR7(3:5)      TO W-4A1-IDORDNR                   
094102                                            WS-IDORDNR7                   
094202                                            W-721-IDORDNR7                
094302         ELSE                                                             
094410           MOVE MID-IDORDNR7           TO MOD-IDORDNR7                    
094502           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDORDNR7-ATTR               
094602           MOVE NEJ TO NYCKLAR-SW                                         
094702         END-IF                                                           
094802       ELSE                                                               
094902         MOVE MFS-RENSA-FAELT          TO MOD-IDORDNR7                    
095002       END-IF                                                             
095102                                                                          
095202       IF MID-IDKOLLI                  NOT = ALL '+'                      
095302         IF MID-IDKOLLI NUMERIC AND                                       
095402           MID-IDKOLLI > ZERO                                             
095502           MOVE MID-IDKOLLI            TO W-IDKOLLI                       
095710                                          W-721-IDKOLLI                   
095802         ELSE                                                             
095910           MOVE MID-IDKOLLI            TO MOD-IDKOLLI                     
096002           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDKOLLI-ATTR                
096102           MOVE NEJ TO NYCKLAR-SW                                         
096202         END-IF                                                           
096302       ELSE                                                               
096402         MOVE MFS-RENSA-FAELT          TO MOD-IDKOLLI                     
096502       END-IF                                                             
096602     END-IF                                                               
096702                                                                          
096802     IF NYCKLAR-FEL                                                       
096910       MOVE W-FEL-758                  TO MED-IDMFSFEL                    
097002       CALL WMEDKONV USING MED-WMEDAREA                                   
097102       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
097202       MOVE MFS-NUM-FAELT-FEL      TO MOD-IDDISTR-ATTR                    
097302                                    MOD-IDKUNDNR-ATTR                     
097402                                    MOD-IDORDNR7-ATTR                     
097502                                    MOD-IDKOLLI-ATTR                      
097602     END-IF                                                               
097702     .                                                                    
097802     EJECT                                                                
097902 BC-KOLLA-KOLLI-KOMB SECTION.                                             
098002                                                                          
098102*--- IDDISTR+IDKUNDNR+IDORDER+IDKOLLI MÅSTE FYLLAS I IHOP                 
098202                                                                          
098302     IF EGEN-MID                                                          
098402       IF (MID-IDDISTR       = ALL '+' AND                                
098502         MID-IDKUNDNR        = ALL '+' AND                                
098602         MID-IDORDNR7        = ALL '+' AND                                
098702         MID-IDKOLLI         = ALL '+')                                   
098802         OR                                                               
098902         (MID-IDDISTR    NOT = ALL '+' AND                                
099002         MID-IDKUNDNR    NOT = ALL '+' AND                                
099102         MID-IDORDNR7    NOT = ALL '+' AND                                
099202         MID-IDKOLLI     NOT = ALL '+')                                   
099302         CONTINUE                                                         
099402       ELSE                                                               
099502         MOVE NEJ TO NYCKLAR-SW                                           
099602       END-IF                                                             
099702     END-IF                                                               
099802     .                                                                    
099902     EJECT                                                                
100002 BD-KOLLA-IDVO       SECTION.                                             
100102                                                                          
100202     MOVE W-IDVO              TO W-IDVO-4539                              
100302     MOVE W-IDDC              TO W-IDDC-4539                              
100402     PERFORM IMS-GU-WDGX4539-40                                           
100502                                                                          
100602     IF SEGMENT-SAKNAS                                                    
100702       MOVE NEJ TO NYCKLAR-SW                                             
100802     END-IF                                                               
100902                                                                          
101002     .                                                                    
101102     EJECT                                                                
101202 C-LAES-VISA-INFO SECTION.                                                
101302     MOVE 'C-LAES-VISA-INFO   '    TO CURRENT-SECTION                     
101402                                                                          
101502     IF MFS-UPDATE                                                        
101602*---    VISA SVAR FRÅGA FRÅN SCANNER                                      
101702*---    ALT  SVAR EFTER UPPDATERING VIA ANTINGEN SKÄRM/SCANNER            
101802                                                                          
101902        IF VISA-SK-STATUS-O-R OR                                          
102002          ADD-DEL-TILL-OPPET-SK                                           
102102          PERFORM S10-VISA-O-R-SKOLLI                                     
102202        ELSE                                                              
102302          IF VISA-VALT-SK OR                                              
102402            ADD-DEL-TILL-VALT-SK OR                                       
102502            AVSLUTA-SK OR                                                 
102602            PRINTA-FLAGGA-ELLER-FS OR                                     
102702            UPPD-ATGARD                                                   
102802            PERFORM S11-VISA-VALT-SKOLLI                                  
102902          END-IF                                                          
103002        END-IF                                                            
103102     ELSE                                                                 
103202*---    ENTER.  VISA SVAR PÅ FRÅGA FRÅN SKÄRM                             
103302                                                                          
103402        IF VISA-SK-STATUS-O-R                                             
103502          PERFORM S10-VISA-O-R-SKOLLI                                     
103602        ELSE                                                              
103702          IF VISA-VALT-SK                                                 
103802            PERFORM S11-VISA-VALT-SKOLLI                                  
103902          ELSE                                                            
104002            MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                            
104102            CALL WMEDKONV USING MED-WMEDAREA                              
104202            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
104302            PERFORM MFS-RENSA-FAELT-IN                                    
104402            PERFORM MFS-RENSA-FAELT-UT                                    
104502            MOVE NEJ                      TO INDATA-SW                    
104602          END-IF                                                          
104702        END-IF                                                            
104802     END-IF                                                               
104902                                                                          
105002     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR                                  
105102                             MOD-IDKUNDNR                                 
105202                             MOD-IDORDNR7                                 
105302                             MOD-IDKOLLI                                  
105402                             MOD-DIKOLLIH-SAMP                            
105502                                                                          
105602     .                                                                    
105702     EJECT                                                                
105802 D-KOLLA-INPUT SECTION.                                                   
105902     MOVE 'D-KOLLA-INPUT      '    TO CURRENT-SECTION                     
106002*--- KOLL VID PF11                                                        
106102                                                                          
106202     MOVE JA  TO INDATA-SW                                                
106302                                                                          
106402                                                                          
106502     IF MID-KDATGSKLI-UP = 'A' OR 'D' OR SPACE OR '+'                     
106602       MOVE MFS-ALFA-FAELT-RAETT       TO MOD-KDATGSKLI-UP-ATTR           
106702       IF MID-KDATGSKLI-UP = '+'                                          
106802         MOVE SPACE                    TO MID-KDATGSKLI-UP                
106902       END-IF                                                             
107002     ELSE                                                                 
107102       MOVE MFS-ALFA-FAELT-FEL         TO MOD-KDATGSKLI-UP-ATTR           
107202       MOVE 001             TO MED-IDMFSFEL                               
107302       CALL WMEDKONV USING MED-WMEDAREA                                   
107402       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
107502       MOVE NEJ                        TO INDATA-SW                       
107602     END-IF                                                               
107702                                                                          
107802     IF MID-FLPRT-ADR = JA OR YES OR NEJ OR SPACE OR '+'                  
107902       MOVE MFS-ALFA-FAELT-RAETT       TO MOD-FLPRT-ADR-ATTR              
108002       IF MID-FLPRT-ADR = '+'                                             
108102         MOVE SPACE                    TO MID-FLPRT-ADR                   
108202       END-IF                                                             
108302     ELSE                                                                 
108402       MOVE MFS-ALFA-FAELT-FEL         TO MOD-FLPRT-ADR-ATTR              
108502       MOVE 001             TO MED-IDMFSFEL                               
108602       CALL WMEDKONV USING MED-WMEDAREA                                   
108702       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
108802       MOVE NEJ                        TO INDATA-SW                       
108902     END-IF                                                               
109002                                                                          
109102     IF MID-FLPRT-FS  = JA OR YES OR NEJ OR SPACE OR '+'                  
109202       MOVE MFS-ALFA-FAELT-RAETT       TO MOD-FLPRT-FS-ATTR               
109302       IF MID-FLPRT-FS = '+'                                              
109402         MOVE SPACE                    TO MID-FLPRT-FS                    
109502       END-IF                                                             
109602     ELSE                                                                 
109702       MOVE MFS-ALFA-FAELT-FEL         TO MOD-FLPRT-FS-ATTR               
109802       MOVE 001             TO MED-IDMFSFEL                               
109902       CALL WMEDKONV USING MED-WMEDAREA                                   
110002       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
110102       MOVE NEJ                        TO INDATA-SW                       
110202     END-IF                                                               
110302                                                                          
110402     IF MID-FLKLAR = JA OR YES OR NEJ OR SPACE OR '+'                     
110502       MOVE MFS-ALFA-FAELT-RAETT       TO MOD-FLKLAR-ATTR                 
110602       IF MID-FLKLAR = '+'                                                
110702         MOVE SPACE                    TO MID-FLKLAR                      
110802       END-IF                                                             
110902     ELSE                                                                 
111002       MOVE MFS-ALFA-FAELT-FEL         TO MOD-FLKLAR-ATTR                 
111102       MOVE 001             TO MED-IDMFSFEL                               
111202       CALL WMEDKONV USING MED-WMEDAREA                                   
111302       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
111402       MOVE NEJ                        TO INDATA-SW                       
111502     END-IF                                                               
111602                                                                          
111702*--- DIKOLLIH-SAMP                                                        
111802                                                                          
111902     IF MID-DIKOLLIH-SAMP NOT = ALL '+'                                   
112002       IF MID-DIKOLLIH-SAMP NUMERIC AND                                   
112102          MID-DIKOLLIH-SAMP > ZERO                                        
112202         MOVE MID-DIKOLLIH-SAMP        TO WS-DIKOLLIH-SAMP                
112302       ELSE                                                               
112402         MOVE MFS-NUM-FAELT-FEL        TO MOD-DIKOLLIH-SAMP-ATTR          
112502         MOVE 001           TO MED-IDMFSFEL                               
112602         CALL WMEDKONV USING MED-WMEDAREA                                 
112702         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
112802         MOVE NEJ                      TO INDATA-SW                       
112902       END-IF                                                             
113002     ELSE                                                                 
113102       MOVE MFS-RENSA-FAELT          TO MOD-DIKOLLIH-SAMP                 
113202     END-IF                                                               
113302                                                                          
113402     IF INDATA-OK                                                         
113502       PERFORM DA-BESTAM-BEHKOD                                           
113602     END-IF                                                               
113702                                                                          
113802                                                                          
113902     IF INDATA-FEL                                                        
114002       PERFORM MFS-ROER-EJ-FAELT-UT                                       
114102       PERFORM MFS-ROER-EJ-FAELT-IN                                       
114202     END-IF                                                               
114302     .                                                                    
114402     EJECT                                                                
114502                                                                          
114602                                                                          
114702 DA-BESTAM-BEHKOD SECTION.                                                
114802                                                                          
114902     MOVE 'DA-BESTAM-BEHKOD   '    TO CURRENT-SECTION                     
115002                                                                          
115102*--- VAD HAR MAN FÖR AVSIKT ATT UTFÖRA?                                   
115202*--- SÄTT BEHKOD  BEROENDE PÅ INMATADE NYCKLAR/ÅTGÄRD/AVSLUT              
115302                                                                          
115402*--- VI BEHÖVER NYCKEL IDKOLLI-SAMP VID AVSLUTA,PRINTA O UPPD-ÅTG         
115502*--- HAR VI DEN INTE I SÖKNYCKELN, TA FRÅN DET VISADE SK VID AVSLU        
115602*--- OCH VID UPPDATERA-ÅTGÄRD                                             
115702                                                                          
115802     IF (MID-FLKLAR = JA OR YES) OR                                       
115902       (MID-KDATGSKLI-UP = 'A' OR 'D') OR                                 
116002       (MID-FLPRT-ADR = JA OR YES) OR                                     
116102       (MID-FLPRT-FS  = JA OR YES)                                        
116202                                                                          
116302       IF NOT SOEKNYCKEL-SK                                               
116402         IF (MID-FLKLAR = JA OR YES) OR                                   
116502           (MID-KDATGSKLI-UP = 'A' OR 'D')                                
116602           IF MID-IDKOLLI-SAMP NUMERIC AND                                
116702             MID-IDKOLLI-SAMP > ZERO                                      
116802             MOVE MID-IDKOLLI-SAMP TO W-IDKOLLI-SAMP                      
116902                                      W-7A1-IDKOLLI-SAMP                  
117002           ELSE                                                           
117102             MOVE 'ANGE SAMKOLLI' TO MOD-TEMFSFEL                         
117202             PERFORM MFS-ROER-EJ-FAELT-IN                                 
117302             PERFORM MFS-ROER-EJ-FAELT-UT                                 
117402             PERFORM MFS-LAES-IN-IGEN                                     
117502             MOVE NEJ TO INDATA-SW                                        
117602           END-IF                                                         
117702         ELSE                                                             
117802             MOVE 'ANGE SAMKOLLI' TO MOD-TEMFSFEL                         
117902             PERFORM MFS-ROER-EJ-FAELT-IN                                 
118002             PERFORM MFS-ROER-EJ-FAELT-UT                                 
118102             PERFORM MFS-LAES-IN-IGEN                                     
118202             MOVE NEJ TO INDATA-SW                                        
118302         END-IF                                                           
118402       END-IF                                                             
118502     END-IF                                                               
118602                                                                          
118702                                                                          
118802*--- IFYLLT AVSLUT = JA - AVSLUTA,STÄNG SKOLLI                            
118902                                                                          
119002     IF INDATA-OK                                                         
119102       IF MID-FLKLAR = JA OR YES                                          
119202         MOVE '1'       TO WS-BEHKOD                                      
119302         PERFORM DAA-KOLLA-AVSLUT                                         
119402       ELSE                                                               
119502         IF MID-KDATGSKLI-UP = 'A' OR 'D'                                 
119602           MOVE '6'           TO WS-BEHKOD                                
119702         ELSE                                                             
119802           IF (MID-FLPRT-ADR = JA OR YES) OR                              
119902             (MID-FLPRT-FS = JA OR YES)                                   
120002             MOVE '7'         TO WS-BEHKOD                                
120102             PERFORM DAC-KOLLA-PRINT                                      
120202           ELSE                                                           
120302                                                                          
120402*---     IFYLLT DIST-KUND-ORDER-KOLLI OCH SKOLLI                          
120502*---     - MAN HAR FÖR AVSIKT ATT LÄGGA/TA BORT KOLLI I SK                
120602*---     - GÄLLER BÅDE STATUS OPEN OCH REOPEN                             
120702*---     - STATUS FÅR EJ VARA S =SHIPPED, MEN CLOSED BLIR REOPEN          
120802*---     - KOLLA ÅTGÄRD I SK, STYR OM TILLÄGG/BORTTAG AV KOLLI I S        
120902                                                                          
121002             IF MID-IDKOLLI-SAMP-IN NUMERIC AND                           
121102               MID-IDKOLLI-SAMP-IN > ZERO   AND                           
121202               MID-IDDISTR      NUMERIC     AND                           
121302               MID-IDDISTR       > ZERO     AND                           
121402               MID-IDKUNDNR     NUMERIC     AND                           
121502              (MID-IDKUNDNR      = ZERO OR                                
121602               MID-IDKUNDNR      > ZERO)    AND                           
121702               MID-IDORDNR7     NUMERIC     AND                           
121802               MID-IDORDNR7      > ZERO     AND                           
121902               MID-IDKOLLI      NUMERIC     AND                           
122002               MID-IDKOLLI       > ZERO                                   
122102               MOVE '3' TO WS-BEHKOD                                      
122202                                                                          
122302*---           HÄMTA IDTRPTNR FRÅN SKOLLI                                 
122402               PERFORM IMS-GU-WDE711-ASEQ                                 
122502               IF SEGMENT-FINNS                                           
122602                 IF SKLI-KDSTASKLI = SHIPPED                              
122702                   MOVE 'SAMLINGSKOLLIT ÄR VALT FÖR SKEPPNING'            
122802                                           TO MOD-TEMFSFEL                
122902                   PERFORM MFS-ROER-EJ-FAELT-IN                           
123002                   MOVE NEJ TO INDATA-SW                                  
123102                 ELSE                                                     
123202                   MOVE SKLI-IDTRPTNR TO WS-IDTRPTNR                      
123402                   MOVE SKLI-KDATGSKLI TO WS-ATGARD                       
123511                                                                          
123611*---               VI LÄSER 701-SEGMENT FÖR ATT FÅ KDFRAKT-SKTRP          
123711                   PERFORM IMS-GNP-WDE701-ASEQ                            
123811                   MOVE SKTV-KDFRAKT-SKTRP TO WS-KDFRAKT-SKTRP            
123902                   PERFORM DAB-KOLLA-IDTRPTNR                             
124002                 END-IF                                                   
124102               ELSE                                                       
124202                 MOVE 'SAMLINGSKOLLI SAKNAS' TO MOD-TEMFSFEL              
124302                 PERFORM MFS-ROER-EJ-FAELT-IN                             
124402                 MOVE NEJ TO INDATA-SW                                    
124502               END-IF                                                     
124602             ELSE                                                         
124702                                                                          
124802*---            IFYLLT DIST-KUND-ORDER-KOLLI OCH SKOLLI EJ IFYLLD         
124902*---            IDTRPT + IDVO IFYLLD                                      
125002*---            MAN VILL ADDERA ALT. DELETA TILL ETT ÖPPET SK,STAT        
125102*---            - SAKNAS ÖPPET SKOLLI VID ADDERA SÅ SKAPA ETT NYTT        
125202*---              KOLLI ADDERAS                                           
125302*---            - ADDERA KOLLI TILL SKOLLI                                
125402                                                                          
125502               IF MID-IDTRPTNR-IN NUMERIC    AND                          
125602                 MID-IDTRPTNR-IN > ZERO      AND                          
125702                 MID-IDVO-IN NUMERIC         AND                          
125802                 MID-IDVO-IN > ZERO          AND                          
125909                 MID-KDFRAKT-SKTRP-IN NUMERIC AND                         
126009                 MID-KDFRAKT-SKTRP-IN                                     
126105                            NOT < ZERO       AND                          
126205                 MID-IDDISTR NUMERIC         AND                          
126305                 MID-IDDISTR > ZERO          AND                          
126405                 MID-IDKUNDNR NUMERIC        AND                          
126505                (MID-IDKUNDNR = ZERO OR                                   
126605                 MID-IDKUNDNR > ZERO)        AND                          
126705                 MID-IDORDNR7 NUMERIC        AND                          
126805                 MID-IDORDNR7 > ZERO         AND                          
126905                 MID-IDKOLLI NUMERIC         AND                          
127005                 MID-IDKOLLI > ZERO                                       
127105                 MOVE '4'                    TO WS-BEHKOD                 
127205                 MOVE MID-IDTRPTNR-IN        TO WS-IDTRPTNR               
127309                 MOVE MID-KDFRAKT-SKTRP-IN   TO WS-KDFRAKT-SKTRP          
127405                                                                          
127505                 PERFORM DAD-LETA-OPPET-SK-KDATGSKLI                      
127605                                                                          
127705                 PERFORM DAB-KOLLA-IDTRPTNR                               
127805               ELSE                                                       
127905                                                                          
128005*---            IFYLLT SKOLLI MEN BLANKT I DIST-KUND-ORDER-KOLLI -        
128105*---            STATUS PÅ SKOLLIT                                         
128205                                                                          
128305                 IF MID-IDKOLLI-SAMP-IN NUMERIC AND                       
128405                   MID-IDKOLLI-SAMP-IN > ZERO AND                         
128505                   (MID-IDDISTR = ALL '+' AND                             
128605                   MID-IDKUNDNR = ALL '+' AND                             
128705                   MID-IDORDNR7 = ALL '+' AND                             
128805                   MID-IDKOLLI = ALL '+')                                 
128905                   MOVE '5' TO WS-BEHKOD                                  
129005                 ELSE                                                     
129105                                                                          
129205*---           IFYLLT TRP-VO ENDAST - VISA ÖPPET SKOLLI OM FINNS          
129305*---                                - SKAPA ROT OM ROT SAKNAS             
129405                                                                          
129505                   IF MID-IDTRPTNR-IN NUMERIC     AND                     
129605                     MID-IDTRPTNR-IN > ZERO       AND                     
129705                     MID-IDVO-IN NUMERIC          AND                     
129805                     MID-IDVO-IN > ZERO           AND                     
129909                     MID-KDFRAKT-SKTRP-IN NUMERIC AND                     
130009                     MID-KDFRAKT-SKTRP-IN NOT < ZERO                      
130105                     MOVE '2' TO WS-BEHKOD                                
130205                   ELSE                                                   
130305*---                 FELMEDDELA 'FEL KOMBINATION'                         
130405                     MOVE 238              TO MED-IDMFSFEL                
130505                     CALL WMEDKONV USING MED-WMEDAREA                     
130605                     MOVE MED-MFSFEL TO MOD-TEMFSFEL                      
130705                     PERFORM MFS-ROER-EJ-FAELT-IN                         
130805                     PERFORM MFS-ROER-EJ-FAELT-UT                         
130905                     PERFORM MFS-LAES-IN-IGEN                             
131005                     MOVE NEJ TO INDATA-SW                                
131105                   END-IF                                                 
131205                 END-IF                                                   
131305               END-IF                                                     
131405             END-IF                                                       
131505           END-IF                                                         
131605         END-IF                                                           
131705       END-IF                                                             
131805     END-IF                                                               
131905                                                                          
132005     .                                                                    
132105     EJECT                                                                
132205                                                                          
132305                                                                          
132405 DAA-KOLLA-AVSLUT          SECTION.                                       
132505     MOVE 'DAA-KOLLA-AVSLUT   '    TO CURRENT-SECTION                     
132605                                                                          
132705*--- AVSIKT: STÄNGA SKOLLI OCH PRINTA KOLLIFLAGGA                         
132805                                                                          
132905     PERFORM IMS-GU-WDE711-ASEQ                                           
133005     IF SEGMENT-FINNS                                                     
133105       IF SKLI-KDSTASKLI = CLOSED OR                                      
133205         SKLI-KDSTASKLI = SHIPPED                                         
133305         MOVE 'SAMLINGSKOLLI REDAN STÄNGT' TO MOD-TEMFSFEL                
133405         PERFORM MFS-ROER-EJ-FAELT-IN                                     
133505         MOVE NEJ                    TO INDATA-SW                         
133605       END-IF                                                             
133705                                                                          
133805*--- VID AVSLUT MÅSTE FINNAS > 1 KOLLI I SK                               
133905       PERFORM IMS-GNP-WDE721-ASEQ                                        
134005       IF SEGMENT-SAKNAS                                                  
134105         MOVE 'SAMLINGSKOLLIT INNEHÅLLER INGA KOLLIN'                     
134205                                     TO MOD-TEMFSFEL                      
134305         PERFORM MFS-ROER-EJ-FAELT-IN                                     
134405         MOVE NEJ                    TO INDATA-SW                         
134505       ELSE                                                               
134605         MOVE SKOR-IDDISTR           TO 434P-MID-IDDISTR                  
134705       END-IF                                                             
134805     ELSE                                                                 
134905         MOVE 'SAMLINGSKOLLI SAKNAS' TO MOD-TEMFSFEL                      
135005         PERFORM MFS-ROER-EJ-FAELT-IN                                     
135105         MOVE NEJ                    TO INDATA-SW                         
135205     END-IF                                                               
135305                                                                          
135405     IF INDATA-OK                                                         
135505       IF MSGI-KDPRTVAL-ADR = SPACE OR ALL '+'                            
135605         MOVE 'AF-PRINTER EJ ANGIVEN' TO MOD-TEMFSFEL                     
135705         PERFORM MFS-ROER-EJ-FAELT-IN                                     
135805         PERFORM MFS-ROER-EJ-FAELT-UT                                     
135905         PERFORM MFS-LAES-IN-IGEN                                         
136005         MOVE MFS-ADD-SAETT-CURSOR TO MOD-KDPRTVAL-ADR-ATTR               
136105         MOVE NEJ                    TO INDATA-SW                         
136205       ELSE                                                               
136305         PERFORM S31-KOLLA-KDPRTVAL-ADR                                   
136405       END-IF                                                             
136505     END-IF                                                               
136605                                                                          
136705     IF INDATA-OK                                                         
136805       IF MSGI-KDPRTVAL-FS  = SPACE OR ALL '+'                            
136905         MOVE 'FS-PRINTER EJ ANGIVEN' TO MOD-TEMFSFEL                     
137005         MOVE MFS-ADD-SAETT-CURSOR TO MOD-KDPRTVAL-FS-ATTR                
137105         PERFORM MFS-ROER-EJ-FAELT-IN                                     
137205         MOVE NEJ                    TO INDATA-SW                         
137305       ELSE                                                               
137405         PERFORM S33-KOLLA-KDPRTVAL-FS                                    
137505       END-IF                                                             
137605     END-IF                                                               
137705                                                                          
137805                                                                          
137905     IF INDATA-OK                                                         
138005       IF MID-KDKOLLI-SAMP = SPACE OR ALL '+'                             
138105         MOVE 'EMBALLAGE EJ ANGIVET' TO MOD-TEMFSFEL                      
138210         MOVE MFS-ADD-SAETT-CURSOR   TO MOD-KDKOLLI-SAMP-ATTR             
138305         PERFORM MFS-ROER-EJ-FAELT-IN                                     
138405         MOVE NEJ                    TO INDATA-SW                         
138505       ELSE                                                               
138610         MOVE SPACE                  TO WS-KDKOLLI                        
138710         MOVE MID-KDKOLLI-SAMP       TO WS-KDKOLLI                        
138810         MOVE WS-KDKOLLI             TO W-KDKOLLI-K5                      
138905                                                                          
139005         PERFORM IMS-GU-WDK501                                            
139105                                                                          
139205         IF SEGMENT-SAKNAS                                                
139305           MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDKOLLI-SAMP-ATTR           
139405           PERFORM MFS-ROER-EJ-FAELT-IN                                   
139510           MOVE NEJ                    TO INDATA-SW                       
139605           MOVE 'EMBALLAGEKOD FEL'     TO MOD-TEMFSFEL                    
139705         ELSE                                                             
139805                                                                          
139905           IF EMB-DIKOLLIH = ZERO                                         
140005                                                                          
140105             IF WS-DIKOLLIH-SAMP > ZERO                                   
140205               MOVE MFS-NUM-FAELT-RAETT  TO MOD-DIKOLLIH-SAMP-ATTR        
140305             ELSE                                                         
140405               MOVE MFS-NUM-FAELT-FEL    TO MOD-DIKOLLIH-SAMP-ATTR        
140505               PERFORM MFS-ROER-EJ-FAELT-IN                               
140605               PERFORM MFS-ROER-EJ-FAELT-UT                               
140705               PERFORM MFS-LAES-IN-IGEN                                   
140805               MOVE MFS-ADD-SAETT-CURSOR TO MOD-DIKOLLIH-SAMP-ATTR        
140905               MOVE NEJ TO INDATA-SW                                      
141005               MOVE 'HÖJD PÅ EMBALLAGE SAKNAS' TO MOD-TEMFSFEL            
141105             END-IF                                                       
141205           ELSE                                                           
141305             IF WS-DIKOLLIH-SAMP > ZERO                                   
141405               MOVE MFS-NUM-FAELT-FEL    TO MOD-DIKOLLIH-SAMP-ATTR        
141505               PERFORM MFS-ROER-EJ-FAELT-IN                               
141605               PERFORM MFS-ROER-EJ-FAELT-UT                               
141705               PERFORM MFS-LAES-IN-IGEN                                   
141805               MOVE MFS-ADD-SAETT-CURSOR TO MOD-DIKOLLIH-SAMP-ATTR        
141905               MOVE NEJ TO INDATA-SW                                      
142005               MOVE 'EMBALLAGE HAR REDAN HÖJD ANGIVEN'                    
142105                                              TO MOD-TEMFSFEL             
142205             ELSE                                                         
142305               MOVE MFS-NUM-FAELT-RAETT TO MOD-DIKOLLIH-SAMP-ATTR         
142405             END-IF                                                       
142505           END-IF                                                         
142605         END-IF                                                           
142705       END-IF                                                             
142805     END-IF                                                               
142905     .                                                                    
143005     EJECT                                                                
143105                                                                          
143205                                                                          
143305 DAB-KOLLA-IDTRPTNR SECTION.                                              
143405     MOVE 'DAB-KOLLA-IDTRPTNR '    TO CURRENT-SECTION                     
143505                                                                          
143605*---       AVSIKT:  ADDERA/TA BORT KOLLI TILL SKOLLI                      
143713*---       KOLLA I E611 ATT KOLLI HAR RÄTT IDTRPTNR ETC                   
143805                                                                          
143913*---       LÄS WDE401 FÖR ATT FÅ NYCKEL IDPRODNR TILL WDE6                
144005                                                                          
144105                                                                          
144205     IF INDATA-OK                                                         
144305                                                                          
144405       PERFORM IMS-GU-WDE401-ASEQ                                         
144505                                                                          
144605       IF SEGMENT-FINNS                                                   
144705         MOVE NEJ   TO TRAFF-IDDC-SW                                      
144805         MOVE NEJ   TO TRAFF-IDKOLLI-SW                                   
144905                                                                          
145005         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                    
145105                       TRAFF-IDKOLLI OR INDATA-FEL                        
145205                                                                          
145305           MOVE NEJ   TO TRAFF-IDDC-SW                                    
145405           IF GOOD-DDC                                                    
145505             IF KORD-IDDC = W-IDDC   AND                                  
145605                KORD-KVORDRAD = ZERO                                      
145705               MOVE JA     TO TRAFF-IDDC-SW                               
145805             END-IF                                                       
145905           ELSE                                                           
146005             IF GOOD-DC                                                   
146105                IF KORD-IDDC = W-IDDC AND                                 
146205                   KORD-KVORDRAD-LEVPL = ZERO                             
146305                  MOVE JA     TO TRAFF-IDDC-SW                            
146405                END-IF                                                    
146505             END-IF                                                       
146605           END-IF                                                         
146705           IF TRAFF-IDDC                                                  
146805             MOVE NEJ                  TO TRAFF-IDKOLLI-SW                
146905             MOVE KORD-IDPRODNR        TO W-IDPRODNR                      
147005                                                                          
147105*---         WDE601 LÄSES FÖR KDFRAKT                                     
147205             PERFORM IMS-GU-WDE601                                        
147305             PERFORM IMS-GU-WDE611                                        
147405             IF SEGMENT-FINNS                                             
147505                                                                          
147605                                                                          
147705               IF  KOLLI-KDKOLSTA NOT       = KLI-PACK                    
147805               OR  KOLLI-IDTRPTNR NOT       = WS-IDTRPTNR                 
147909               OR (WS-KDFRAKT-SKTRP         > ZERO AND                    
148009                   VORD-KDFRAKT NOT         = WS-KDFRAKT-SKTRP)           
148105               OR ((WS-ATGARD               = 'A' OR SPACE) AND           
148205                   KOLLI-FLUTLAST NOT       = JA )                        
148305               OR (WS-ATGARD                = 'D' AND                     
148405                   KOLLI-FLUTLAST           = JA )                        
148505                                                                          
148605                 MOVE 'FEL TRANSPORT, FK ELLER STATUS PÅ KOLLIT'          
148710                                           TO MOD-TEMFSFEL                
148805                 PERFORM MFS-ROER-EJ-FAELT-IN                             
148905                 MOVE NEJ                  TO INDATA-SW                   
149005               ELSE                                                       
149105                 MOVE JA                   TO TRAFF-IDKOLLI-SW            
149205               END-IF                                                     
149305             ELSE                                                         
149405               PERFORM IMS-GN-WDE401-ASEQ                                 
149505             END-IF                                                       
149605           ELSE                                                           
149705             PERFORM IMS-GN-WDE401-ASEQ                                   
149805           END-IF                                                         
149905         END-PERFORM                                                      
150005         IF EJ-TRAFF-IDKOLLI                                              
150105           MOVE 'FEL TRANSPORT, FK ELLER STATUS PÅ KOLLIT'                
150205                                          TO MOD-TEMFSFEL                 
150305           PERFORM MFS-ROER-EJ-FAELT-IN                                   
150405           MOVE NEJ                       TO INDATA-SW                    
150505         END-IF                                                           
150605       ELSE                                                               
150705         MOVE W-FEL-758                 TO MED-IDMFSFEL                   
150805         CALL WMEDKONV USING MED-WMEDAREA                                 
150905         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
151005         MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-ATTR                    
151105                                      MOD-IDKUNDNR-ATTR                   
151205                                      MOD-IDORDNR7-ATTR                   
151305                                      MOD-IDKOLLI-ATTR                    
151405         MOVE NEJ                  TO INDATA-SW                           
151505       END-IF                                                             
151605     END-IF                                                               
151705     .                                                                    
151805     EJECT                                                                
151905 DAC-KOLLA-PRINT           SECTION.                                       
152005     MOVE 'DAC-KOLLA-PRINT    '    TO CURRENT-SECTION                     
152105                                                                          
152205*--- AVSIKT: PRINTA KOLLIFLAGGA OCH/ELLER FÖLJESEDEL                      
152305*---         PÅ NYTT FÖR STÄNGT KOLLI                                     
152405                                                                          
152505     PERFORM IMS-GU-WDE711-ASEQ                                           
152605     IF SEGMENT-FINNS                                                     
152705       IF SKLI-KDSTASKLI = CLOSED OR                                      
152805         SKLI-KDSTASKLI = SHIPPED                                         
152905         CONTINUE                                                         
153005       ELSE                                                               
153105         MOVE 'SAMLINGSKOLLIT ÄR INTE STÄNGT' TO MOD-TEMFSFEL             
153205         PERFORM MFS-ROER-EJ-FAELT-IN                                     
153305         MOVE NEJ                    TO INDATA-SW                         
153405       END-IF                                                             
153505                                                                          
153605       PERFORM IMS-GNP-WDE721-ASEQ                                        
153705       IF SEGMENT-SAKNAS                                                  
153805         MOVE 'SAMLINGSKOLLIT INNEHÅLLER INGA KOLLIN'                     
153905                                      TO MOD-TEMFSFEL                     
154005         PERFORM MFS-ROER-EJ-FAELT-IN                                     
154105         MOVE NEJ                    TO INDATA-SW                         
154205       ELSE                                                               
154305         MOVE SKOR-IDDISTR           TO 434P-MID-IDDISTR                  
154405       END-IF                                                             
154505     ELSE                                                                 
154605         MOVE 'SAMLINGSKOLLI SAKNAS' TO MOD-TEMFSFEL                      
154705         PERFORM MFS-ROER-EJ-FAELT-IN                                     
154805         MOVE NEJ                    TO INDATA-SW                         
154905     END-IF                                                               
155005                                                                          
155105     IF INDATA-OK                                                         
155205       IF (MID-FLPRT-ADR = JA OR YES)                                     
155305         IF MSGI-KDPRTVAL-ADR = SPACE                                     
155405           MOVE 'AF-PRINTER EJ ANGIVEN' TO MOD-TEMFSFEL                   
155505           PERFORM MFS-ROER-EJ-FAELT-IN                                   
155605           MOVE NEJ                  TO INDATA-SW                         
155705         ELSE                                                             
155805           PERFORM S31-KOLLA-KDPRTVAL-ADR                                 
155905         END-IF                                                           
156005       END-IF                                                             
156105     END-IF                                                               
156205                                                                          
156305     IF INDATA-OK                                                         
156405       IF (MID-FLPRT-FS = JA OR YES)                                      
156505         IF MSGI-KDPRTVAL-FS = SPACE                                      
156605           MOVE 'FS-PRINTER EJ ANGIVEN' TO MOD-TEMFSFEL                   
156705           PERFORM MFS-ROER-EJ-FAELT-IN                                   
156805           MOVE NEJ                  TO INDATA-SW                         
156905         ELSE                                                             
157005           PERFORM S33-KOLLA-KDPRTVAL-FS                                  
157105         END-IF                                                           
157205       END-IF                                                             
157305     END-IF                                                               
157405                                                                          
157505     .                                                                    
157605     EJECT                                                                
157705                                                                          
157805 DAD-LETA-OPPET-SK-KDATGSKLI   SECTION.                                   
157905     MOVE 'DAD-LETA-OPPET-SK  '    TO CURRENT-SECTION                     
158005                                                                          
158105*--- ADD/DEL KOLLI TILL ÖPPET SK DÅ SKOLLI EJ ANGIVET                     
158205*--- INFÖR KOLL AV FLUTLAST                                               
158305*--- BEHÖVER VI VETA KDATSKLI I DET ÖPPNA SKOLLIT                         
158405*--- SAKNAS WD711-SEGMENT, ÄR DET EN ADD SOM ÄR PÅ G                      
158505*--- SEGMENTET HAR BARA INTE HUNNIT SKAPAS ÄN                             
158605                                                                          
158705     MOVE NEJ                        TO TRAFF-OPPET-SK-SW                 
158805     MOVE 'A'                        TO WS-ATGARD                         
158905     PERFORM IMS-GU-WDE701                                                
159005                                                                          
159105     IF SEGMENT-FINNS                                                     
159305       PERFORM IMS-GNP-WDE711                                             
159405                                                                          
159505       IF SEGMENT-FINNS                                                   
159705         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                    
159805                       TRAFF-OPPET-SK                                     
159905                                                                          
160005           IF SKLI-KDSTASKLI = OPENED                                     
160105             MOVE JA                  TO TRAFF-OPPET-SK-SW                
160205             MOVE SKLI-KDATGSKLI      TO WS-ATGARD                        
160305           END-IF                                                         
160405           PERFORM IMS-GNP-WDE711                                         
160505         END-PERFORM                                                      
160605       END-IF                                                             
160705     END-IF                                                               
160805                                                                          
160905     .                                                                    
161005     EJECT                                                                
161105                                                                          
161205                                                                          
161305 E-UPPDATERA SECTION.                                                     
161405     MOVE 'E-UPPDATERA        '    TO CURRENT-SECTION                     
161505                                                                          
161605     IF PRINTA-FLAGGA-ELLER-FS                                            
161705       PERFORM EA-PRINT-FLAGGA-ELLER-FS                                   
161805     ELSE                                                                 
161905                                                                          
162005       IF UPPD-ATGARD                                                     
162105         PERFORM EB-UPPD-KDATGSKLI-I-SK                                   
162205       ELSE                                                               
162305                                                                          
162405         IF AVSLUTA-SK                                                    
162505           PERFORM EC-AVSLUTA-SK                                          
162605         ELSE                                                             
162705                                                                          
162805           IF ADD-DEL-TILL-VALT-SK                                        
162905             PERFORM ED-ADD-DEL-TILL-VALT-SK                              
163005           ELSE                                                           
163105                                                                          
163205             IF ADD-DEL-TILL-OPPET-SK                                     
163305               PERFORM EE-ADD-DEL-TILL-OPPET-SK                           
163405             END-IF                                                       
163505           END-IF                                                         
163605         END-IF                                                           
163705       END-IF                                                             
163805     END-IF                                                               
163905                                                                          
164005                                                                          
164105     .                                                                    
164205     EJECT                                                                
164305 EA-PRINT-FLAGGA-ELLER-FS        SECTION.                                 
164405     MOVE 'EA-PRINT-FLAGGA    '    TO CURRENT-SECTION                     
164505                                                                          
164605*--- 711-SEGM INLÄST TIDIGARE.                                            
164705                                                                          
164805     IF MID-FLPRT-ADR = 'Y' OR 'J'                                        
164905       PERFORM S30-PRINT-FLAGGA                                           
165005       MOVE INF-PRINT-STARTED TO MED-IDMFSINF                             
165105       CALL WMEDKONV USING MED-WMEDAREA                                   
165205       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
165305     END-IF                                                               
165405                                                                          
165505     IF MID-FLPRT-FS  = 'Y' OR 'J'                                        
165605       PERFORM S32-PRINT-FS                                               
165705       MOVE INF-PRINT-STARTED TO MED-IDMFSINF                             
165805       CALL WMEDKONV USING MED-WMEDAREA                                   
165905       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
166005     END-IF                                                               
166105                                                                          
166205     .                                                                    
166305     EJECT                                                                
166405 EB-UPPD-KDATGSKLI-I-SK SECTION.                                          
166505     MOVE 'EB-UPPD-KDATGSKLI-I-SK ' TO CURRENT-SECTION                    
166605                                                                          
166705******************************************************************        
166805*    HANTERING ÅTGÄRD, KDATGSKLI                                          
166905*    VID SKAPA NYTT SK            SÄTT = A                                
167005*    EFTER ADD/DEL FÖR STATUS R   SÄTT = BLANK                            
167105*    EFTER ADD     FÖR STATUS O   ORÖRD (= A)                             
167205*    EFTER DEL     FÖR STATUS O   SÄTT = A                                
167305*    VID STÄNG SK                 SÄTT = BLANK                            
167405******************************************************************        
167505                                                                          
167605*--- ÄNDRA GÄLLANDE ÅTGÄRD I SK,  TILL A ELLER D                          
167705                                                                          
167805     PERFORM IMS-GHU-WDE711-ASEQ                                          
167905                                                                          
168005     IF SEGMENT-FINNS                                                     
168105                                                                          
168205*LB 160927 VI ÅTERÖPPNAR EJ. GÖRS FÖRST VID INMATN. AV KOLLI              
168305*      IF SKLI-KDSTASKLI = CLOSED                                         
168405*        MOVE REOPENED TO SKLI-KDSTASKLI                                  
168505*      END-IF                                                             
168605                                                                          
168705*LB 160927 VI SÄTTER BARA ÅTG A EL D.                                     
168805*LB 160927 BLANK FINNS BARA INITIALT FÖR ETT REOPENED SK                  
168905*      IF MID-KDATGSKLI-UP = 'D'                                          
169005*        MOVE 'D'   TO SKLI-KDATGSKLI                                     
169105*      ELSE                                                               
169205*        MOVE SPACE TO SKLI-KDATGSKLI                                     
169305*      END-IF                                                             
169405                                                                          
169505       MOVE MID-KDATGSKLI-UP   TO SKLI-KDATGSKLI                          
169605       PERFORM IMS-REPL-WDE711-ASEQ                                       
169705                                                                          
169805       IF SEGMENT-SAKNAS                                                  
169905         MOVE 'SAMLINGSKOLLI SAKNAS' TO MOD-TEMFSFEL                      
170005         PERFORM MFS-ROER-EJ-FAELT-IN                                     
170105         PERFORM MFS-ROER-EJ-FAELT-UT                                     
170205         MOVE NEJ TO INDATA-SW                                            
170305       ELSE                                                               
170405         IF MID-KDATGSKLI-UP = 'D'                                        
170505           MOVE 'KLART ATT REGISTRERA BORTTAG AV KOLLI'                   
170605                 TO MOD-TEMFSINF                                          
170705         ELSE                                                             
170805           MOVE 'KLART ATT REGISTRERA TILLÄGG AV KOLLI'                   
170905                 TO MOD-TEMFSINF                                          
171005         END-IF                                                           
171105       END-IF                                                             
171205     ELSE                                                                 
171305                                                                          
171405       MOVE 'SAMLINGSKOLLI SAKNAS' TO MOD-TEMFSFEL                        
171505       PERFORM MFS-ROER-EJ-FAELT-IN                                       
171605       PERFORM MFS-ROER-EJ-FAELT-UT                                       
171705       MOVE NEJ TO INDATA-SW                                              
171805     END-IF                                                               
171905                                                                          
172005     .                                                                    
172105     EJECT                                                                
172205                                                                          
172305 EC-AVSLUTA-SK            SECTION.                                        
172405     MOVE 'EC-AVSLUTA-SK      '    TO CURRENT-SECTION                     
172505                                                                          
172605*---       BERÄKNA BRUTTOVIKT NETTOVIKT, VOLYM OCH ANT KOLLI I SK         
172705*---       ÄNDRA STATUS I SK TILL C                                       
172706*---       UPPDATERA TIREGDAT, PGA RENSNING AV SK ÄLDRE ÄN 30 DGR         
172805*---       PRINTA KOLLIFLAGGA                                             
172905                                                                          
173005     PERFORM ECA-SUMMERA-KOLLIN                                           
173105                                                                          
173205*--- LÄGG TILL VIKT FÖR SAMLINGSKOLLITS EMBALLAGE                         
173305                                                                          
173405     COMPUTE WS-VKORDBTO-SAMP ROUNDED =                                   
173505             WS-VKORDBTO-SAMP + EMB-VKTARA                                
173605                                                                          
173705     PERFORM  IMS-GHU-WDE711                                              
173805                                                                          
173905*--- OM EMB-DIKOLLIH = NOLL ANVÄND HÖJD FRÅN MID                          
174005*--- I VOLYMBERÄKNING                                                     
174105                                                                          
174205     IF EMB-DIKOLLIH = ZERO                                               
174305       MOVE WS-DIKOLLIH-SAMP  TO W-EMB-DIKOLLIH                           
174405     ELSE                                                                 
174505       MOVE EMB-DIKOLLIH       TO W-EMB-DIKOLLIH                          
174605     END-IF                                                               
174705                                                                          
174805*--- WDK5 INLÄST TIDIGARE                                                 
174905     COMPUTE SKLI-VLKOLLIB-SAMP ROUNDED =                                 
175005       EMB-DIKOLLIL * W-EMB-DIKOLLIH * EMB-DIKOLLIB / 1000000             
175105*--------------------------------------- EMB   BREDD, HÖJD OCH            
175205*--------------------------------------- LÄNGD ANGIVNA I CM MEDAN         
175305*--------------------------------------- BRUTTOVOLYM I KUBIK M.           
175405                                                                          
175505     MOVE WS-VKORDBTO-SAMP     TO SKLI-VKKOLLIB-SAMP                      
175605     MOVE WS-VKORDNTO-SAMP     TO SKLI-VKKOLLIN-SAMP                      
175705     MOVE MID-KDKOLLI-SAMP     TO SKLI-KDKOLLI-SAMP                       
175805                                                                          
175905     MOVE WS-ANTAL-KOLLIN-I-SAMP TO SKLI-KVKOLLI-SAMP                     
176005     MOVE CLOSED               TO SKLI-KDSTASKLI                          
176105     MOVE SPACE                TO SKLI-KDATGSKLI                          
176205     MOVE WS-FLFARLIG          TO SKLI-FLFARLIG                           
176305     MOVE WS-KDFRAKT-1         TO SKLI-KDFRAKT (1)                        
176405     MOVE WS-KDFRAKT-2         TO SKLI-KDFRAKT (2)                        
176505     MOVE WS-KDFRAKT-3         TO SKLI-KDFRAKT (3)                        
176506     MOVE WS-DATUM             TO SKLI-TIREGDAT                           
176605                                                                          
176705     PERFORM  IMS-REPL-WDE711                                             
176805                                                                          
176905     PERFORM  S30-PRINT-FLAGGA                                            
177005     PERFORM  S32-PRINT-FS                                                
177105                                                                          
177205*--- NÄR SK STÄNGS MÅSTE SÖKNYCKEL VARA SK, EJ TRP-VO                     
177305     MOVE MID-IDKOLLI-SAMP     TO MOD-IDKOLLI-SAMP-UT                     
177405     MOVE MFS-RENSA-FAELT      TO MOD-IDTRPTNR-UT                         
177505                                  MOD-IDVO-UT                             
177609                                  MOD-KDFRAKT-SKTRP-UT                    
177705                                                                          
177805     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
177905     CALL WMEDKONV USING MED-WMEDAREA                                     
178005     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
178105     .                                                                    
178205     EJECT                                                                
178305                                                                          
178405 ECA-SUMMERA-KOLLIN      SECTION.                                         
178505     MOVE 'ECA-SUMMERA-KOLLIN '    TO CURRENT-SECTION                     
178605                                                                          
178705     MOVE ZERO                        TO WS-ANTAL-KOLLIN-I-SAMP           
178805                                         WS-VKORDNTO-SAMP                 
178905                                         WS-VKORDBTO-SAMP                 
179005                                         WS-KDFRAKT-1                     
179105                                         WS-KDFRAKT-2                     
179205                                         WS-KDFRAKT-3                     
179305     MOVE NEJ                         TO WS-FLFARLIG                      
179405                                                                          
179505     PERFORM IMS-GU-WDE711-ASEQ                                           
179605                                                                          
179705*--- LÄS OCH SPARA 701- NYCKEL FÖR SENARE REPL AV 711                     
179805     PERFORM IMS-GNP-WDE701-ASEQ                                          
179905     MOVE SKTV-IDTRPTNR              TO W-IDTRPTNR                        
180009     MOVE SKTV-KDFRAKT-SKTRP         TO W-KDFRAKT-SKTRP                   
180105     MOVE SKTV-IDVO                  TO W-IDVO                            
180205     MOVE SKTV-IDDC                  TO W-IDDC                            
180305                                        W-7A1-IDDC                        
180405                                                                          
180505     IF SEGMENT-FINNS                                                     
180605       PERFORM IMS-GNP-WDE721-ASEQ                                        
180705       IF SEGMENT-FINNS                                                   
180805         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                       
180905           ADD +1                     TO WS-ANTAL-KOLLIN-I-SAMP           
181005                                                                          
181105*----      INGÅENDE KOLLINS BRUTTOVIKT BLIR SK NETTOVIKT                  
181205           ADD SKOR-VKORDBTO-KOLLI    TO WS-VKORDNTO-SAMP                 
181305           ADD SKOR-VKORDBTO-KOLLI    TO WS-VKORDBTO-SAMP                 
181405                                                                          
181505           PERFORM ECAB-KDFRAKT                                           
181605                                                                          
181705           IF WS-FLFARLIG = NEJ                                           
181805             PERFORM ECAA-KOLLA-FLFARLIG                                  
181905           END-IF                                                         
182005*---       ÄNDRA STATUS PÅ E611 FÖR ALLA INGÅENDE KOLLIN ETT O ETT        
182105           PERFORM S21-UPPD-E611-STATUS-C                                 
182205           PERFORM IMS-GNP-WDE721-ASEQ                                    
182305         END-PERFORM                                                      
182405       END-IF                                                             
182505     ELSE                                                                 
182605       MOVE 'SAMLINGSKOLLI SAKNAS' TO MOD-TEMFSFEL                        
182705       PERFORM MFS-ROER-EJ-FAELT-IN                                       
182805       PERFORM MFS-ROER-EJ-FAELT-UT                                       
182905       MOVE NEJ TO INDATA-SW                                              
183005     END-IF                                                               
183105                                                                          
183205     .                                                                    
183305     EJECT                                                                
183405                                                                          
183505 ECAA-KOLLA-FLFARLIG     SECTION.                                         
183605                                                                          
183705     MOVE 'ECAA-KOLLA-FLFARLIG'    TO CURRENT-SECTION                     
183805                                                                          
183905     MOVE SKOR-IDPRODNR                 TO W-IDPRODNR                     
184005     MOVE SKOR-IDKOLLI                  TO W-IDKOLLI                      
184105                                           W-721-IDKOLLI                  
184205     PERFORM IMS-GU-WDE611                                                
184305                                                                          
184405     IF SEGMENT-FINNS                                                     
184505       MOVE +1 TO IX                                                      
184605       PERFORM UNTIL WS-FLFARLIG = JA OR IX > 10                          
184705         IF KOLLI-IDPSN (IX) > ZERO                                       
184805           MOVE JA                      TO WS-FLFARLIG                    
184905         END-IF                                                           
185005         ADD +1 TO IX                                                     
185105       END-PERFORM                                                        
185205     END-IF                                                               
185305                                                                          
185405     .                                                                    
185505     EJECT                                                                
185605                                                                          
185705 ECAB-KDFRAKT     SECTION.                                                
185805                                                                          
185905     MOVE 'ECAB-KDFRAKT       '    TO CURRENT-SECTION                     
186005                                                                          
186105     IF SKOR-KDFRAKT = WS-KDFRAKT-1 OR                                    
186210       SKOR-KDFRAKT  = WS-KDFRAKT-2 OR                                    
186310       SKOR-KDFRAKT  = WS-KDFRAKT-3                                       
186405       CONTINUE                                                           
186505     ELSE                                                                 
186605       IF WS-KDFRAKT-1 = ZERO                                             
186705         MOVE SKOR-KDFRAKT    TO WS-KDFRAKT-1                             
186805       ELSE                                                               
186905         IF WS-KDFRAKT-2 = ZERO                                           
187005           MOVE SKOR-KDFRAKT  TO WS-KDFRAKT-2                             
187105         ELSE                                                             
187205           IF WS-KDFRAKT-3 = ZERO                                         
187305             MOVE SKOR-KDFRAKT TO WS-KDFRAKT-3                            
187405           END-IF                                                         
187505         END-IF                                                           
187605       END-IF                                                             
187705     END-IF                                                               
187805     .                                                                    
187905     EJECT                                                                
188005                                                                          
188105 ED-ADD-DEL-TILL-VALT-SK  SECTION.                                        
188205                                                                          
188305     MOVE 'ED-ADD-DEL-TILL-VALT-SK' TO CURRENT-SECTION                    
188405                                                                          
188505*--- ADD/DELETE KOLLI TILL SK BEROENDE PÅ ÅTGÄRD I SK                     
188605                                                                          
188705     PERFORM IMS-GHU-WDE711-ASEQ                                          
188805                                                                          
188905*--- OM SK ÄR STÄNGT, GÖR REOPEN                                          
189005     IF SKLI-KDSTASKLI = CLOSED                                           
189105       MOVE REOPENED       TO SKLI-KDSTASKLI                              
189205                                                                          
189305*---   SLÅ AV FÄLT SOM SATTS VID CLOSE                                    
189405       MOVE ZERO           TO SKLI-VLKOLLIB-SAMP                          
189505                              SKLI-VKKOLLIB-SAMP                          
189605                              SKLI-VKKOLLIN-SAMP                          
189705                              SKLI-KVKOLLI-SAMP                           
189805                              SKLI-KDFRAKT (1)                            
189905                              SKLI-KDFRAKT (2)                            
190005                              SKLI-KDFRAKT (3)                            
190105       MOVE SPACE          TO SKLI-KDKOLLI-SAMP                           
190205                              SKLI-FLFARLIG                               
190305                                                                          
190405       PERFORM IMS-REPL-WDE711-ASEQ                                       
190505       PERFORM S22-UPPD-ALLA-E611-STATUS-R                                
190605       PERFORM IMS-GHU-WDE711-ASEQ                                        
190705     END-IF                                                               
190805                                                                          
190905*--- LÄS OCH SPARA 701- NYCKEL FÖR SENARE GHU AV 721                      
191005     PERFORM IMS-GNP-WDE701-ASEQ                                          
191105     MOVE SKTV-IDTRPTNR              TO W-IDTRPTNR                        
191209     MOVE SKTV-KDFRAKT-SKTRP         TO W-KDFRAKT-SKTRP                   
191305     MOVE SKTV-IDVO                  TO W-IDVO                            
191405     MOVE SKTV-IDDC                  TO W-IDDC                            
191505                                        W-7A1-IDDC                        
191605                                                                          
191705     IF SEGMENT-FINNS                                                     
191805                                                                          
191905*---   STATUS ÄR OPEN ELLER REOPEN                                        
192005                                                                          
192105       IF SKLI-KDATGSKLI = 'D'                                            
192205         MOVE NEJ              TO WS-E611-SK-KOPPL                        
192305         PERFORM IMS-GHU-WDE721                                           
192405         IF SEGMENT-FINNS                                                 
192505           PERFORM IMS-DEL-WDE721                                         
192605           IF SEGMENT-SAKNAS                                              
192705             CONTINUE                                                     
192805           ELSE                                                           
192905                                                                          
193005*---       SLÅ AV ÅTGÄRD FÖR BÅDE OPEN O REOPNAT SK VID DEL AV KOL        
193105             PERFORM IMS-GHU-WDE711-ASEQ                                  
193205                                                                          
193305             IF SKLI-KDSTASKLI = OPENED                                   
193405               MOVE 'A'   TO SKLI-KDATGSKLI                               
193505             ELSE                                                         
193605               MOVE SPACE TO SKLI-KDATGSKLI                               
193705             END-IF                                                       
193805             PERFORM IMS-REPL-WDE711-ASEQ                                 
193905                                                                          
194005             PERFORM S20-UPPD-E611-KOLLI                                  
194105             MOVE 'KOLLI BORTTAGET UR SAMLINGSKOLLI'                      
194205                                      TO MOD-TEMFSINF                     
194206                                                                          
194207*---         SISTA KOLLIT TAS BORT UR ETT REOPENED SK                     
194208*---         DVS SK BLIVIT TOMT, DELETA ÄVEN SK                           
194209                                                                          
194210             IF SKLI-KDSTASKLI = REOPENED                                 
194220              PERFORM EDA-EV-DELETE-REOPENED-SK                           
194230             END-IF                                                       
194240                                                                          
194305           END-IF                                                         
194405         ELSE                                                             
194505           MOVE 'KOLLI SAKNAS I SAMLINGSKOLLIT' TO MOD-TEMFSFEL           
194605           PERFORM MFS-ROER-EJ-FAELT-IN                                   
194705           PERFORM MFS-ROER-EJ-FAELT-UT                                   
194805           MOVE NEJ TO INDATA-SW                                          
194905         END-IF                                                           
195005       ELSE                                                               
195105                                                                          
195205*---     LÄGG TILL KOLLI, ÅTGÄRD ÄR A ELLER BLANK                         
195305*---     STATUS ÄR O ELLER R                                              
195405                                                                          
195505         IF SKLI-KDATGSKLI = 'A'                                          
195605                                                                          
195705*---       SLÅ AV ÅTGÄRD FÖR ETT REOPNAT SK VID ADD AV KOLLI              
195805           PERFORM IMS-GHU-WDE711-ASEQ                                    
195905           IF SKLI-KDSTASKLI = REOPENED                                   
196005             MOVE SPACE TO SKLI-KDATGSKLI                                 
196105           END-IF                                                         
196205           PERFORM IMS-REPL-WDE711-ASEQ                                   
196305                                                                          
196405           MOVE JA             TO WS-E611-SK-KOPPL                        
196505           PERFORM S12-SKAPA-WDE721                                       
196605           PERFORM IMS-ISRT-WDE721                                        
196705           IF SEGMENT-FINNS-REDAN                                         
196805             MOVE 251                  TO MED-IDMFSFEL                    
196905             CALL WMEDKONV USING MED-WMEDAREA                             
197005             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
197105             PERFORM MFS-ROER-EJ-FAELT-IN                                 
197205             PERFORM MFS-ROER-EJ-FAELT-UT                                 
197305             MOVE NEJ TO INDATA-SW                                        
197405           ELSE                                                           
197505             IF DUPL-KEY-SEQ-INDEX                                        
197605               MOVE 'KOLLI REDAN I ANNAT SAMKOLLI' TO MOD-TEMFSFEL        
197705               PERFORM MFS-ROER-EJ-FAELT-IN                               
197805               PERFORM MFS-ROER-EJ-FAELT-UT                               
197905               PERFORM MFS-LAES-IN-IGEN                                   
198005               MOVE NEJ TO INDATA-SW                                      
198105             ELSE                                                         
198205               PERFORM S20-UPPD-E611-KOLLI                                
198305               MOVE INF-UPDATE-DONE TO MED-IDMFSINF                       
198405               CALL WMEDKONV USING MED-WMEDAREA                           
198505               MOVE MED-MFSINF TO MOD-TEMFSINF                            
198605             END-IF                                                       
198705           END-IF                                                         
198805         ELSE                                                             
198905           MOVE 'ANGE ÅTGÄRD A ELLER D' TO MOD-TEMFSFEL                   
199005           PERFORM MFS-ROER-EJ-FAELT-IN                                   
199105           PERFORM MFS-ROER-EJ-FAELT-UT                                   
199205           PERFORM MFS-LAES-IN-IGEN                                       
199305           MOVE NEJ TO INDATA-SW                                          
199405         END-IF                                                           
199505       END-IF                                                             
199605                                                                          
199705     ELSE                                                                 
199805       MOVE 'SAMLINGSKOLLI SAKNAS' TO MOD-TEMFSFEL                        
199905       PERFORM MFS-ROER-EJ-FAELT-IN                                       
200005       PERFORM MFS-ROER-EJ-FAELT-UT                                       
200105       MOVE NEJ TO INDATA-SW                                              
200205     END-IF                                                               
200305     .                                                                    
200405     EJECT                                                                
200505                                                                          
200605                                                                          
200705                                                                          
200805 EDA-EV-DELETE-REOPENED-SK    SECTION.                                    
200905     MOVE 'EDA-EV-DELETE-REOPENED-SK ' TO CURRENT-SECTION                 
200906                                                                          
200907*--- ETT REOPENED SK SOM TÖMTS HELT PÅ INGÅENDE KOLLIN DELETAS            
200908                                                                          
200909     PERFORM IMS-GU-WDE711-ASEQ                                           
238805                                                                          
238905     IF SEGMENT-FINNS                                                     
239005       PERFORM IMS-GNP-WDE721-ASEQ                                        
239105                                                                          
239205       IF SEGMENT-SAKNAS                                                  
239206         PERFORM IMS-GHU-WDE711                                           
239207        IF SEGMENT-FINNS                                                  
239208          PERFORM IMS-DEL-WDE711                                          
239209          MOVE                                                            
239210         'SISTA KOLLIT BORTTAGET, ÄVEN SAMLINGSKOLLIT BORTTAGET'          
239220                                      TO MOD-TEMFSINF                     
239230        END-IF                                                            
239240       END-IF                                                             
239250     END-IF                                                               
239251                                                                          
239252     .                                                                    
239253     EJECT                                                                
239254                                                                          
239255                                                                          
239256                                                                          
239257 EE-ADD-DEL-TILL-OPPET-SK     SECTION.                                    
239258     MOVE 'EE-ADD-DEL-TILL-OPPET-SK  ' TO CURRENT-SECTION                 
239259                                                                          
239260*--- ADD/DEL KOLLI TILL ÖPPET SK DÅ SKOLLI EJ ANGIVET                     
239261*--- SAKNAS ÖPPET SKOLLI, SKAPA  ETT NYTT                                 
239262                                                                          
239263     PERFORM IMS-GU-WDE701                                                
239264                                                                          
239265     IF SEGMENT-FINNS                                                     
239266       MOVE NEJ        TO TRAFF-OPPET-SK-SW                               
239267       PERFORM IMS-GNP-WDE711                                             
239268                                                                          
239269       IF SEGMENT-FINNS                                                   
239270         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                    
239271                       TRAFF-OPPET-SK                                     
239272                                                                          
239273           IF SKLI-KDSTASKLI = OPENED                                     
239274             MOVE JA                  TO TRAFF-OPPET-SK-SW                
239275             MOVE SKLI-IDKOLLI-SAMP   TO W-IDKOLLI-SAMP                   
239276                                         W-7A1-IDKOLLI-SAMP               
239277                                         MOD-IDKOLLI-SAMP                 
239278           END-IF                                                         
239279           IF TRAFF-OPPET-SK                                              
239280             CONTINUE                                                     
239281           ELSE                                                           
239282             PERFORM IMS-GNP-WDE711                                       
239283           END-IF                                                         
239284         END-PERFORM                                                      
239285       END-IF                                                             
239286                                                                          
239287       IF TRAFF-OPPET-SK                                                  
239288         IF SKLI-KDATGSKLI = 'D'                                          
239289            PERFORM EEB-DELETE-KOLLI-UR-SK                                
239290         ELSE                                                             
239291           PERFORM EEA-ADD-KOLLI-TILL-SK                                  
239292         END-IF                                                           
239293       ELSE                                                               
239294         PERFORM EEC-SKAPA-SKOLLI                                         
239295         IF INDATA-OK                                                     
239296           MOVE SKLI-IDKOLLI-SAMP    TO W-IDKOLLI-SAMP                    
239297                                            W-7A1-IDKOLLI-SAMP            
239298                                            MOD-IDKOLLI-SAMP              
239299           PERFORM EEA-ADD-KOLLI-TILL-SK                                  
239300         END-IF                                                           
239301       END-IF                                                             
239302                                                                          
239303     END-IF                                                               
239304     .                                                                    
239305     EJECT                                                                
239306 EEA-ADD-KOLLI-TILL-SK SECTION.                                           
239307     MOVE 'EEA-ADD-KOLLI-SK      ' TO CURRENT-SECTION                     
239308                                                                          
239309       IF INDATA-OK                                                       
239310*---   NU HAR VI ETT ÖPPET SKOLLI ATT JOBBA MED                           
239311*---   LÄGG TILL KOLLIT OCH UPPD WDE611                                   
239312                                                                          
239313         MOVE JA           TO WS-E611-SK-KOPPL                            
239314                                                                          
239315         PERFORM S12-SKAPA-WDE721                                         
239316         PERFORM IMS-ISRT-WDE721                                          
239317         IF SEGMENT-FINNS-REDAN                                           
239318           MOVE 'KOLLIT REDAN I SAMLINGSKOLLIT' TO MOD-TEMFSFEL           
239319           PERFORM MFS-ROER-EJ-FAELT-IN                                   
239320           PERFORM MFS-ROER-EJ-FAELT-UT                                   
239321           PERFORM MFS-LAES-IN-IGEN                                       
239322           MOVE NEJ TO INDATA-SW                                          
239323         ELSE                                                             
239324           IF DUPL-KEY-SEQ-INDEX                                          
239325             MOVE 'KOLLIT REDAN I ANNAT SAMKOLLI' TO MOD-TEMFSFEL         
239326             PERFORM MFS-ROER-EJ-FAELT-IN                                 
239327             PERFORM MFS-ROER-EJ-FAELT-UT                                 
239328             PERFORM MFS-LAES-IN-IGEN                                     
239329             MOVE NEJ TO INDATA-SW                                        
239330           ELSE                                                           
239331             PERFORM S20-UPPD-E611-KOLLI                                  
239332             MOVE INF-UPDATE-DONE TO MED-IDMFSINF                         
239333             CALL WMEDKONV USING MED-WMEDAREA                             
239334             MOVE MED-MFSINF TO MOD-TEMFSINF                              
239335           END-IF                                                         
239336         END-IF                                                           
239337       ELSE                                                               
239338         MOVE 'TRANSPORT/VO  SAKNAS' TO MOD-TEMFSFEL                      
239339         PERFORM MFS-ROER-EJ-FAELT-IN                                     
239340         PERFORM MFS-ROER-EJ-FAELT-UT                                     
239341         PERFORM MFS-LAES-IN-IGEN                                         
239342         MOVE NEJ TO INDATA-SW                                            
239343       END-IF                                                             
239344     .                                                                    
239345                                                                          
239346 EEB-DELETE-KOLLI-UR-SK   SECTION.                                        
239347     MOVE 'EEB-DELETE-KOLLI-UR-SK' TO CURRENT-SECTION                     
239348*---   711-SEGM INLÄST                                                    
239349*---   STATUS ÄR OPEN                                                     
239350                                                                          
239351     MOVE NEJ                  TO WS-E611-SK-KOPPL                        
239352     PERFORM IMS-GHU-WDE721                                               
239353     IF SEGMENT-FINNS                                                     
239354       PERFORM IMS-DEL-WDE721                                             
239355       IF SEGMENT-SAKNAS                                                  
239356         CONTINUE                                                         
239357       ELSE                                                               
239358                                                                          
239359*---   SLÅ AV ÅTGÄRD VID DELETE AV KOLLI                                  
239360         PERFORM IMS-GHU-WDE711-ASEQ                                      
239361         MOVE 'A'       TO SKLI-KDATGSKLI                                 
239362         PERFORM IMS-REPL-WDE711-ASEQ                                     
239363         PERFORM S20-UPPD-E611-KOLLI                                      
239364         MOVE 'KOLLI BORTTAGET UR SAMLINGSKOLLI'                          
239365                                  TO MOD-TEMFSINF                         
239366       END-IF                                                             
239367     ELSE                                                                 
239368       MOVE 'KOLLI SAKNAS I SAMLINGSKOLLIT' TO MOD-TEMFSFEL               
239369       PERFORM MFS-ROER-EJ-FAELT-IN                                       
239370       PERFORM MFS-ROER-EJ-FAELT-UT                                       
239371       MOVE NEJ TO INDATA-SW                                              
239372     END-IF                                                               
239373     .                                                                    
239374                                                                          
239375                                                                          
239376     EJECT                                                                
239377 EEC-SKAPA-SKOLLI SECTION.                                                
239378     MOVE 'EEC-SKAPA-SKOLLI      ' TO CURRENT-SECTION                     
239379                                                                          
239380*--- SKAPA ETT NYTT ÖPPET SKOLLI (701-SEGM INLÄST)                        
239381                                                                          
239382     INITIALIZE SKLI-WDE711                                               
239383                                                                          
239384     PERFORM EECA-SKAPA-NASTA-IDKOLLI-SAMP                                
239385                                                                          
239386     IF INDATA-OK                                                         
239387                                                                          
239388       MOVE WS-SKLI-IDKOLLI-SAMP   TO SKLI-IDKOLLI-SAMP                   
239389       MOVE W-IDDC                 TO SKLI-IDDC                           
239390       MOVE KOLLI-ADFLGEO          TO SKLI-ADFLGEO                        
239391       MOVE KOLLI-ADFLOMR          TO SKLI-ADFLOMR                        
239392       MOVE KOLLI-ADRUTNIV         TO SKLI-ADRUTNIV                       
239393       MOVE KOLLI-IDTRPTNR         TO SKLI-IDTRPTNR                       
239394       MOVE NEJ                    TO SKLI-FLFARLIG                       
239395       MOVE 'A'                    TO SKLI-KDATGSKLI                      
239396       MOVE OPENED                 TO SKLI-KDSTASKLI                      
239397       MOVE WS-DATUM               TO SKLI-TIREGDAT                       
239398*---   MOVE SKTV-KDFRAKT-SKTRP     TO SKLI-KDFRAKT-SKTRP                  
239399                                                                          
239400       PERFORM IMS-ISRT-WDE711                                            
239401                                                                          
239402*---   OM LÖPNR 'GÅTT RUNT' OCH REDAN UPPTAGET                            
239403*---   FÖRSÖK MED NÄSTA I SERIEN                                          
239404                                                                          
239405       PERFORM UNTIL SEGMENT-FINNS                                        
239406         PERFORM EECA-SKAPA-NASTA-IDKOLLI-SAMP                            
239407         MOVE WS-SKLI-IDKOLLI-SAMP TO SKLI-IDKOLLI-SAMP                   
239408         PERFORM IMS-ISRT-WDE711                                          
239409       END-PERFORM                                                        
239410     END-IF                                                               
239411                                                                          
239412     .                                                                    
239413 EECA-SKAPA-NASTA-IDKOLLI-SAMP        SECTION.                            
239414     MOVE 'EECA-SKAPA-NASTA-IDKOL' TO CURRENT-SECTION                     
239415                                                                          
239416     MOVE W-IDVO                   TO WS-SKLI-IDVO                        
239417     PERFORM EECAA-HAMTA-SERIENR                                          
239418                                                                          
239419     IF INDATA-OK                                                         
239420       MOVE WS-SKLI-IDKOLLI-SAMP-X TO WS-SKLI-IDKOLLI-SAMP                
239421     END-IF                                                               
239422                                                                          
239423     .                                                                    
239424     EJECT                                                                
239425                                                                          
239426 EECAA-HAMTA-SERIENR  SECTION.                                            
239427     MOVE 'EEAAAA-HAMTA-SERIENR   ' TO CURRENT-SECTION                    
239428                                                                          
239429     MOVE W-IDVO              TO W-IDVO-4539                              
239430     MOVE W-IDDC              TO W-IDDC-4539                              
239431     PERFORM IMS-GHU-WDGX4539-40                                          
239432                                                                          
239433     IF SEGMENT-FINNS                                                     
239434       MOVE 4540-IDKLISAM-AKT TO WS-SKLI-LOPNR                            
239435                                                                          
239436       IF 4540-IDKLISAM-AKT = 4540-IDKLISAM-MAX                           
239437         MOVE 4540-IDKLISAM-MIN TO 4540-IDKLISAM-AKT                      
239438       ELSE                                                               
239439         ADD +1               TO 4540-IDKLISAM-AKT                        
239440       END-IF                                                             
239441                                                                          
239442       PERFORM IMS-REPL-WDGX4540                                          
239443     ELSE                                                                 
239444       MOVE 'VO EJ FÖRBERETT FÖR SAMLINGSKOLLI' TO MOD-TEMFSFEL           
239445       PERFORM MFS-ROER-EJ-FAELT-IN                                       
239446       PERFORM MFS-ROER-EJ-FAELT-UT                                       
239447       MOVE NEJ TO INDATA-SW                                              
239448     END-IF                                                               
239449     .                                                                    
239450     EJECT                                                                
239451     EJECT                                                                
239452 G-SATT-BEHKOD  SECTION.                                                  
239453     MOVE 'G-SATT-BEHKOD         ' TO CURRENT-SECTION                     
239454                                                                          
239455     MOVE JA  TO INDATA-SW                                                
239456                                                                          
239457*--- ENTER OCH IFYLLT SKOLLI                                              
239458*---                        - VISA STATUS PÅ SKOLLIT                      
239459                                                                          
239460     IF SOEKNYCKEL-SK                                                     
239461        MOVE '5'           TO WS-BEHKOD                                   
239462     ELSE                                                                 
239463                                                                          
239464*---   ENTER OCH ENDAST IFYLLT TRP-VO-DC                                  
239465*---                  - VISA ÖPPET SKOLLI OM FINNS                        
239466*---                  - SKAPA ROT OM ROT SAKNAS                           
239467                                                                          
239468       IF SOEKNYCKEL-TRP-VO                                               
239469          MOVE '2' TO WS-BEHKOD                                           
239470        END-IF                                                            
239471     END-IF                                                               
239472     .                                                                    
239473     EJECT                                                                
239474 S10-VISA-O-R-SKOLLI SECTION.                                             
239475     MOVE 'S10-VISA-O-R-SKOLLI   ' TO CURRENT-SECTION                     
239476                                                                          
239477*--- ENDAST IFYLLT TRP-VO-DC                                              
239478*---                  - VISA ÖPPET/ÅTERÖPPNAT SKOLLI OM FINNS             
239479*---                  - SKAPA ROT OM ROT SAKNAS (INGET SK SKAPAS)         
239480                                                                          
239481     PERFORM IMS-GU-WDE701                                                
239482     IF SEGMENT-SAKNAS                                                    
239483       INITIALIZE             SKTV-WDE701                                 
239484       MOVE W-IDTRPTNR        TO SKTV-IDTRPTNR                            
239485       MOVE W-IDVO            TO SKTV-IDVO                                
239486       MOVE W-IDDC            TO SKTV-IDDC                                
239487       MOVE W-KDFRAKT-SKTRP   TO SKTV-KDFRAKT-SKTRP                       
239488       PERFORM IMS-ISRT-WDE701                                            
239489       IF VISA-SK-STATUS-O-R                                              
239490         MOVE 'KLART FÖR ATT REGISTRERA I ÖPPET SKOLLI'                   
239491                                          TO MOD-TEMFSINF                 
239492       END-IF                                                             
239493     ELSE                                                                 
239494                                                                          
239495*---   LÄS SKOLLI MED STATUS OPEN                                         
239496       PERFORM IMS-GNP-WDE711                                             
239497       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
239498         OR TRAFF-STATUS-O-R                                              
239499         IF SKLI-KDSTASKLI = OPENED                                       
239500           MOVE JA                    TO TRAFF-STATUS-O-R-SW              
239501           MOVE SKLI-IDKOLLI-SAMP     TO W-IDKOLLI-SAMP                   
239502                                       W-7A1-IDKOLLI-SAMP                 
239503                                       MOD-IDKOLLI-SAMP                   
239504           MOVE SKLI-KDSTASKLI        TO MOD-KDSTASKLI                    
239505           MOVE SKLI-KDATGSKLI        TO MOD-KDATGSKLI                    
239506           IF VISA-SK-STATUS-O-R                                          
239507             MOVE 'KLART FÖR ATT REGISTRERA I ÖPPET SKOLLI'               
239508                                        TO MOD-TEMFSINF                   
239509           END-IF                                                         
239510         END-IF                                                           
239511         PERFORM IMS-GNP-WDE711                                           
239512       END-PERFORM                                                        
239513                                                                          
239514       IF EJ-TRAFF-STATUS-O-R AND                                         
239515         VISA-SK-STATUS-O-R                                               
239516*---   INTE FEL ATT DET SAKNAS SK. KAN BEHÖVAS ETT NYTT                   
239517                                                                          
239518         MOVE 'KLART FÖR ATT REGISTRERA I NYTT SKOLLI'                    
239519                                          TO MOD-TEMFSINF                 
239520*---     MOVE 'INGET ÖPPET SKOLLI FINNS'                                  
239521*---                                      TO MOD-TEMFSFEL                 
239522       END-IF                                                             
239523                                                                          
239524     END-IF                                                               
239525     .                                                                    
239526     EJECT                                                                
239527 S11-VISA-VALT-SKOLLI SECTION.                                            
239528     MOVE 'S11-VISA-VALT-SKOLLI   ' TO CURRENT-SECTION                    
239529                                                                          
239530*--- LÄS GIVET SKOLLI OAVSETT STATUS.                                     
239531     PERFORM IMS-GU-WDE711-ASEQ                                           
239532                                                                          
239533     IF SEGMENT-FINNS                                                     
239534       MOVE SKLI-IDKOLLI-SAMP             TO MOD-IDKOLLI-SAMP             
239535                                                                          
239536       IF SKLI-KDSTASKLI = SHIPPED                                        
239537         MOVE CLOSED                      TO MOD-KDSTASKLI                
239538       ELSE                                                               
239539         MOVE SKLI-KDSTASKLI              TO MOD-KDSTASKLI                
239540       END-IF                                                             
239541                                                                          
239542       MOVE SKLI-KDATGSKLI                TO MOD-KDATGSKLI                
239543       IF VISA-VALT-SK                                                    
239544         IF SKLI-KDSTASKLI = SHIPPED                                      
239545           MOVE 'SAMLINGSKOLLIT ÄR VALT FÖR SKEPPNING'                    
239546                                          TO MOD-TEMFSINF                 
239547         ELSE                                                             
239548           MOVE 'KLART FÖR ATT REGISTRERA I SAMLINGSKOLLIT'               
239549                                          TO MOD-TEMFSINF                 
239550         END-IF                                                           
239551       END-IF                                                             
239552     ELSE                                                                 
239553                                                                          
239554       IF VISA-VALT-SK                                                    
239555         MOVE 'SAMLINGSKOLLI SAKNAS'      TO MOD-TEMFSFEL                 
239556       END-IF                                                             
239557                                                                          
239558*---   OM SK SAKNAS PGA ATT VI PRECIS DELETAT ETT TÖMT                    
239559*---   REOPNAT SK FINNS REDAN ETT MEDDELANDE I MOD-TEMFSFEL               
239560*---   (WS-BEHKOD = ADD-DEL-TILL-VALT-SK)                                 
239561                                                                          
239562       MOVE NEJ                           TO INDATA-SW                    
239563                                                                          
239564     END-IF                                                               
239565                                                                          
239566     .                                                                    
239567     EJECT                                                                
239568 S12-SKAPA-WDE721     SECTION.                                            
239569     MOVE 'S12-SKAPA-WDE721       ' TO CURRENT-SECTION                    
239570                                                                          
239571     INITIALIZE SKOR-WDE721                                               
239572                                                                          
239573     MOVE WS-IDDISTR            TO SKOR-IDDISTR                           
239574     MOVE WS-IDKUNDNR           TO SKOR-IDKUNDNR                          
239575     MOVE WS-IDORDNR7           TO SKOR-IDORDNR7                          
239576     MOVE W-721-IDKOLLI         TO SKOR-IDKOLLI                           
239577     MOVE VORD-IDPRODNR         TO SKOR-IDPRODNR                          
239578     MOVE W-IDDC                TO SKOR-IDDC                              
239579     MOVE KOLLI-IDTRPTNR        TO SKOR-IDTRPTNR                          
239580     MOVE VORD-KDFRAKT          TO SKOR-KDFRAKT                           
239581     MOVE KOLLI-VKORDNTO-KOLLI  TO SKOR-VKORDNTO-KOLLI                    
239582     MOVE KOLLI-VKORDBTO-KOLLI  TO SKOR-VKORDBTO-KOLLI                    
239583     MOVE KOLLI-VLORDBTO-KOLLI  TO SKOR-VLORDBTO-KOLLI                    
239584                                                                          
239585     .                                                                    
239586     EJECT                                                                
239587 S20-UPPD-E611-KOLLI  SECTION.                                            
239588     MOVE 'S20-UPPD-E611-KOLLI    ' TO CURRENT-SECTION                    
239589                                                                          
239590*--  ANTINGEN HAR KOLLI LAGTS I ELLER TAGITS UR SK                        
239591*--  WDE711 FINNS INLÄST/REPLAC-ATS                                       
239592                                                                          
239593     PERFORM IMS-GHU-WDE611                                               
239594                                                                          
239595     IF WS-E611-SK-KOPPL = NEJ                                            
239596                                                                          
239597*---   TA BORT KOPPLING TILL SK                                           
239598       MOVE ZERO                   TO KOLLI-IDKOLLI-SAMP                  
239599       MOVE JA                     TO KOLLI-FLUTLAST                      
239600       MOVE SPACE                  TO KOLLI-KDSTASKLI                     
239601     ELSE                                                                 
239602                                                                          
239603*---   LÄGG TILL KOPPLING TILL SK                                         
239604*---   STATUS KAN VARA OPEN/REOPEN, TA FRÅN SKLI-                         
239605                                                                          
239606       MOVE W-IDKOLLI-SAMP         TO KOLLI-IDKOLLI-SAMP                  
239607       MOVE NEJ                    TO KOLLI-FLUTLAST                      
239608       MOVE SKLI-KDSTASKLI         TO KOLLI-KDSTASKLI                     
239609     END-IF                                                               
239610                                                                          
239611     PERFORM IMS-REPL-WDE611                                              
239612                                                                          
239613     .                                                                    
239614 S21-UPPD-E611-STATUS-C SECTION.                                          
239615     MOVE 'S21-UPPD-E611-STATUS-C ' TO CURRENT-SECTION                    
239616                                                                          
239617*--  SAMKOLLIT HAR STÄNGTS                                                
239618*--  BYT STATUS PÅ WDE611 PÅ ALLA KOLLIN SOM INGÅR I SAMKOLLIT            
239619*--  ETT I TAGET EFTERSOM VI ÄNDÅ HÅLLER PÅ ATT SNURRA IGENOM             
239620*--  WDE721-SEGMENTEN VID CLOSE                                           
239621                                                                          
239622     MOVE SKOR-IDPRODNR                 TO W-IDPRODNR                     
239623     MOVE SKOR-IDKOLLI                  TO W-IDKOLLI                      
239624     PERFORM IMS-GHU-WDE611                                               
239625                                                                          
239626     IF SEGMENT-FINNS                                                     
239627       MOVE CLOSED                      TO  KOLLI-KDSTASKLI               
239628       PERFORM IMS-REPL-WDE611                                            
239629     END-IF                                                               
239630     .                                                                    
239631                                                                          
239632 S22-UPPD-ALLA-E611-STATUS-R SECTION.                                     
239633     MOVE 'S22-UPPD-ALLA-E611-STATUS-R ' TO CURRENT-SECTION               
239634                                                                          
239635*--  SAMKOLLIT HAR BLIVIT REOPEN                                          
239636*--  BYT STATUS PÅ WDE611 PÅ ALLA KOLLIN SOM INGÅR I SAMKOLLIT            
239637                                                                          
239638     MOVE W-IDPRODNR       TO WS-IDPRODNR                                 
239639     MOVE W-IDKOLLI        TO WS-IDKOLLI                                  
239640     PERFORM IMS-GU-WDE711-ASEQ                                           
239641                                                                          
239642     IF SEGMENT-FINNS                                                     
239643       PERFORM IMS-GNP-WDE721-ASEQ                                        
239644                                                                          
239645       IF SEGMENT-FINNS                                                   
239646         PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                       
239647           MOVE SKOR-IDPRODNR           TO W-IDPRODNR                     
239648           MOVE SKOR-IDKOLLI            TO W-IDKOLLI                      
239649           PERFORM IMS-GHU-WDE611                                         
239705                                                                          
239805           IF SEGMENT-FINNS                                               
239905             MOVE REOPENED              TO  KOLLI-KDSTASKLI               
240005             PERFORM IMS-REPL-WDE611                                      
240105           END-IF                                                         
240205           PERFORM IMS-GNP-WDE721-ASEQ                                    
240305         END-PERFORM                                                      
240405       END-IF                                                             
240505     END-IF                                                               
240605     MOVE WS-IDPRODNR       TO W-IDPRODNR                                 
240705     MOVE WS-IDKOLLI        TO W-IDKOLLI                                  
240805     .                                                                    
240905     EJECT                                                                
241005                                                                          
241105 S30-PRINT-FLAGGA     SECTION.                                            
241205     MOVE 'S30-PRINT-FLAGGA       ' TO CURRENT-SECTION                    
241305                                                                          
241405*--- ANROP PRINTPROGRAM W4034P00                                          
241505*--- 434P-MID-IDDISTR REDAN IFYLLD                                        
241605                                                                          
241705*--- SAMKOLLITS IDDC SKICKAS.                                             
241805*--- IDDC FÖR PRINTER ÄR HÅRDKODAT = 11 I W4034P00                        
241905                                                                          
242005     MOVE W-IDDC                   TO 434P-MID-IDDC                       
242105     MOVE W-IDKOLLI-SAMP           TO 434P-MID-IDKOLLI-SAMP               
242205     MOVE MSGI-KDPRTVAL-ADR        TO 434P-MID-KDPRTVAL                   
242305                                                                          
242405     COMPUTE 434P-KVLL        =                                           
242505             LENGTH OF 434P-MID-W4I34P01 + 17                             
242605                                                                          
242705     MOVE LOW-VALUE                TO 434P-Z1                             
242805     MOVE LOW-VALUE                TO 434P-Z2                             
242905     MOVE MFS-KDMFSFOR             TO 434P-KDMFSFOR                       
243005                                                                          
243105     PERFORM IMS-PURGE-ALT434P-MSG                                        
243205     .                                                                    
243305                                                                          
243405                                                                          
243505     EJECT                                                                
243605 S31-KOLLA-KDPRTVAL-ADR   SECTION.                                        
243705     MOVE 'S31-KOLLA-KDPRTVAL-ADR        ' TO CURRENT-SECTION             
243805                                                                          
243905     MOVE '4'                  TO WS-SYSTDEL-ADR                          
244005     MOVE 'KF'                 TO WS-LISTTYP-ADR                          
244105     MOVE MSGI-IDDC            TO WS-DC-ADR                               
244205     MOVE MSGI-KDPRTVAL-ADR    TO WS-KDPRTVAL-ADR                         
244305                                                                          
244405     MOVE 001                  TO PRT-KDCALL                              
244505     MOVE WS-IDPRTLST-ADR      TO PRT-IDPRTLST                            
244605                                                                          
244705     CALL W006PRT USING PRT-W006PRT                                       
244805                                                                          
244905*--  KOLLA ATT PRINTER FINNS                                              
245005     IF PRT-KDSVAR = RAETT                                                
245105       CONTINUE                                                           
245205     ELSE                                                                 
245305       MOVE ERR-WRONG-PRINTER TO MED-IDMFSFEL                             
245405       CALL WMEDKONV USING MED-WMEDAREA                                   
245505       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
245605       PERFORM MFS-ROER-EJ-FAELT-IN                                       
245705       PERFORM MFS-ROER-EJ-FAELT-UT                                       
245805       PERFORM MFS-LAES-IN-IGEN                                           
245905       MOVE MFS-ADD-SAETT-CURSOR TO MOD-KDPRTVAL-ADR-ATTR                 
246005       MOVE NEJ TO INDATA-SW                                              
246105     END-IF                                                               
246205                                                                          
246305     .                                                                    
246405                                                                          
246505 S32-PRINT-FS         SECTION.                                            
246605     MOVE 'S32-PRINT-FS           ' TO CURRENT-SECTION                    
246705                                                                          
246805*--- ANROP PRINTPROGRAM W4034S00                                          
246905                                                                          
247005*--- SAMKOLLITS IDDC SKICKAS.                                             
247105*--- IDDC FÖR PRINTER ÄR HÅRDKODAT = 11 I W4034S00                        
247205                                                                          
247305     MOVE W-IDDC                   TO 434S-MID-IDDC                       
247405     MOVE W-IDKOLLI-SAMP           TO 434S-MID-IDKOLLI-SAMP               
247505     MOVE MSGI-KDPRTVAL-FS         TO 434S-MID-KDPRTVAL                   
247605                                                                          
247705     COMPUTE 434S-KVLL        =                                           
247805             LENGTH OF 434S-MID-W4I34S01 + 17                             
247905                                                                          
248005     MOVE LOW-VALUE                TO 434S-Z1                             
248105     MOVE LOW-VALUE                TO 434S-Z2                             
248205     MOVE MFS-KDMFSFOR             TO 434S-KDMFSFOR                       
248305                                                                          
248405     PERFORM IMS-PURGE-ALT434S-MSG                                        
248505                                                                          
248605     .                                                                    
248705     EJECT                                                                
248805 S33-KOLLA-KDPRTVAL-FS    SECTION.                                        
248905     MOVE 'S33-KOLLA-KDPRTVAL-FS         ' TO CURRENT-SECTION             
249005                                                                          
249105     MOVE '4'                  TO WS-SYSTDEL-FS                           
249205     MOVE 'FS'                 TO WS-LISTTYP-FS                           
249305     MOVE MSGI-IDDC            TO WS-DC-FS                                
249405     MOVE MSGI-KDPRTVAL-FS     TO WS-KDPRTVAL-FS                          
249505                                                                          
249605     MOVE 001                  TO PRT-KDCALL                              
249705     MOVE WS-IDPRTLST-FS       TO PRT-IDPRTLST                            
249805                                                                          
249905     CALL W006PRT USING PRT-W006PRT                                       
250005                                                                          
250105*--  KOLLA ATT PRINTER FINNS                                              
250205     IF PRT-KDSVAR = RAETT                                                
250305       CONTINUE                                                           
250405     ELSE                                                                 
250505       MOVE ERR-WRONG-PRINTER TO MED-IDMFSFEL                             
250605       CALL WMEDKONV USING MED-WMEDAREA                                   
250705       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
250805       PERFORM MFS-ROER-EJ-FAELT-IN                                       
250905       PERFORM MFS-ROER-EJ-FAELT-UT                                       
251005       PERFORM MFS-LAES-IN-IGEN                                           
251105       MOVE MFS-ADD-SAETT-CURSOR TO MOD-KDPRTVAL-FS-ATTR                  
251205       MOVE NEJ TO INDATA-SW                                              
251305     END-IF                                                               
251405                                                                          
251505     .                                                                    
251605     EJECT                                                                
251705 MFS-RENSA-FAELT-UT SECTION.                                              
251805                                                                          
251905*    --- ALLA UTDATA-FÄLT                                                 
252005     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-SAMP                             
252105                                MOD-KDSTASKLI                             
252205                                MOD-KDATGSKLI                             
252305     .                                                                    
252405     SKIP3                                                                
252505 MFS-RENSA-FAELT-IN SECTION.                                              
252605     MOVE 'MFS-RENSA-FAELT-IN    ' TO CURRENT-SECTION                     
252705                                                                          
252805*    --- ALLA INDATA-FÄLT                                                 
252905     MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-IN                              
253005                                MOD-IDVO-IN                               
253109                                MOD-KDFRAKT-SKTRP-IN                      
253205                                MOD-IDKOLLI-SAMP-IN                       
253305                                MOD-KDATGSKLI-UP                          
253405                                MOD-KDKOLLI-SAMP                          
253505                                MOD-DIKOLLIH-SAMP                         
253605                                MOD-KDPRTVAL-ADR                          
253705                                MOD-KDPRTVAL-FS                           
253805                                MOD-FLPRT-ADR                             
253905                                MOD-FLPRT-FS                              
254005                                MOD-FLKLAR                                
254105                                MOD-IDDISTR                               
254205                                MOD-IDKUNDNR                              
254305                                MOD-IDORDNR7                              
254405                                MOD-IDKOLLI                               
254505     .                                                                    
254605     EJECT                                                                
254705 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
254805                                                                          
254905*    --- ALLA UTDATA-FÄLT                                                 
255005     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKOLLI-SAMP                           
255105                                  MOD-KDSTASKLI                           
255205                                  MOD-KDATGSKLI                           
255305     .                                                                    
255405     SKIP3                                                                
255505 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
255605     MOVE 'MFS-ROER-EJ-FAELT-IN    ' TO CURRENT-SECTION                   
255705                                                                          
255805*    --- ALLA INDATA-FÄLT                                                 
255905     MOVE MFS-ROER-EJ-FAELT TO  MOD-IDTRPTNR-IN                           
256005                                MOD-IDVO-IN                               
256109                                MOD-KDFRAKT-SKTRP-IN                      
256205                                MOD-IDKOLLI-SAMP-IN                       
256305                                MOD-KDATGSKLI-UP                          
256405                                MOD-KDKOLLI-SAMP                          
256505                                MOD-DIKOLLIH-SAMP                         
256605                                MOD-KDPRTVAL-ADR                          
256705                                MOD-KDPRTVAL-FS                           
256805                                MOD-FLPRT-ADR                             
256905                                MOD-FLPRT-FS                              
257005                                MOD-FLKLAR                                
257105                                MOD-IDDISTR                               
257205                                MOD-IDKUNDNR                              
257305                                MOD-IDORDNR7                              
257405                                MOD-IDKOLLI                               
257505     .                                                                    
257605     EJECT                                                                
257705                                                                          
257805                                                                          
257905 MFS-FORM-ATTR SECTION.                                                   
258005                                                                          
258105*    --- ALLA INDATA-FÄLT                                                 
258205     MOVE MFS-FORMATETS-ATTR TO     MOD-KDATGSKLI-UP-ATTR                 
258305                                    MOD-KDKOLLI-SAMP-ATTR                 
258405                                    MOD-DIKOLLIH-SAMP-ATTR                
258505                                    MOD-KDPRTVAL-ADR-ATTR                 
258605                                    MOD-KDPRTVAL-FS-ATTR                  
258705                                    MOD-FLPRT-ADR-ATTR                    
258805                                    MOD-FLPRT-FS-ATTR                     
258905                                    MOD-FLKLAR-ATTR                       
259005                                    MOD-IDDISTR-ATTR                      
259105                                    MOD-IDKUNDNR-ATTR                     
259205                                    MOD-IDORDNR7-ATTR                     
259305                                    MOD-IDKOLLI-ATTR                      
259405     .                                                                    
259505     SKIP2                                                                
259605 MFS-LAES-IN-IGEN SECTION.                                                
259705                                                                          
259805*    --- ALLA INDATA-FÄLT                                                 
259905                                                                          
260005     MOVE MFS-ADD-LAES-IN-FAELT TO    MOD-KDATGSKLI-UP-ATTR               
260105                                      MOD-KDKOLLI-SAMP-ATTR               
260205                                      MOD-DIKOLLIH-SAMP-ATTR              
260305                                      MOD-KDPRTVAL-ADR-ATTR               
260405                                      MOD-KDPRTVAL-FS-ATTR                
260505                                      MOD-FLPRT-ADR-ATTR                  
260605                                      MOD-FLPRT-FS-ATTR                   
260705                                      MOD-FLKLAR-ATTR                     
260805                                      MOD-IDDISTR-ATTR                    
260905                                      MOD-IDKUNDNR-ATTR                   
261005                                      MOD-IDORDNR7-ATTR                   
261105                                      MOD-IDKOLLI-ATTR                    
261205     .                                                                    
261305                                                                          
261405     EJECT                                                                
261505* --- IMS SEKTIONER ---                                                   
261605     SKIP3                                                                
261705 IMS-GET-MSG SECTION.                                                     
261805     MOVE 'IMS-GET-MSG'    TO CURRENT-IMS-SECTION                         
261905                                                                          
262005     MOVE '  QC' TO GODK-STATUSKODER                                      
262105     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
262205     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
262305     PERFORM IMS-STATUSKONTROLL                                           
262405     .                                                                    
262505     SKIP3                                                                
262605 IMS-INSERT-MSG SECTION.                                                  
262705     MOVE 'IMS-INSERT-MSG'    TO CURRENT-IMS-SECTION                      
262805                                                                          
262905     IF MSGI-IDLAND-SPR = 'SE'                                            
263005       MOVE '0' TO MFS-KDHUVOMR                                           
263105     END-IF                                                               
263205     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
263305     MOVE SPACE TO GODK-STATUSKODER                                       
263405     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
263505     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
263605     PERFORM IMS-STATUSKONTROLL                                           
263705     .                                                                    
263805 IMS-PURGE-ALT434P-MSG SECTION.                                           
263905     MOVE 'IMS-PURGE-ALT434P-MSG'    TO CURRENT-IMS-SECTION               
264005                                                                          
264105     MOVE '    ' TO GODK-STATUSKODER                                      
264205     CALL CBLTDLI USING PURG                                              
264305                        ALT434P-PCB                                       
264405                        434P-MSG-IO-AREA                                  
264505     MOVE ALT434P-STATUS-CODE TO STATUS-WS                                
264605     PERFORM IMS-STATUSKONTROLL                                           
264705     .                                                                    
264805     EJECT                                                                
264905 IMS-PURGE-ALT434S-MSG SECTION.                                           
265005     MOVE 'IMS-PURGE-ALT434S-MSG'    TO CURRENT-IMS-SECTION               
265105                                                                          
265205     MOVE '    ' TO GODK-STATUSKODER                                      
265305     CALL CBLTDLI USING PURG                                              
265405                        ALT434S-PCB                                       
265505                        434S-MSG-IO-AREA                                  
265605     MOVE ALT434S-STATUS-CODE TO STATUS-WS                                
265705     PERFORM IMS-STATUSKONTROLL                                           
265805     .                                                                    
265905     EJECT                                                                
266005                                                                          
266105 IMS-GU-WDE701 SECTION.                                                   
266205                                                                          
266305     MOVE 'IMS-GU-WDE701 '    TO CURRENT-IMS-SECTION                      
266405                                                                          
266505     STRING 'WDE701  (WDE701KY =' W-WDE701KY ')'                          
266605          DELIMITED BY SIZE INTO SSA1                                     
266705                                                                          
266805     MOVE '  GE' TO GODK-STATUSKODER                                      
266905     CALL CBLTDLI USING GU WDE7-PCB DLI-IO-WDE701 SSA1                    
267005     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
267105     PERFORM IMS-STATUSKONTROLL                                           
267205     .                                                                    
267305     EJECT                                                                
267405                                                                          
267505 IMS-ISRT-WDE701  SECTION.                                                
267605     MOVE 'IMS-ISRT-WDE701 '    TO CURRENT-IMS-SECTION                    
267705                                                                          
267805     MOVE   'WDE701 '           TO   SSA1                                 
267905                                                                          
268005     MOVE '  II'                TO GODK-STATUSKODER                       
268105     CALL CBLTDLI USING ISRT WDE7-PCB DLI-IO-WDE701 SSA1                  
268205     MOVE WDE7-STATUS-CODE      TO STATUS-WS                              
268305     PERFORM IMS-STATUSKONTROLL                                           
268405     .                                                                    
268505     EJECT                                                                
268605 IMS-GU-WDE711 SECTION.                                                   
268705     MOVE 'IMS-GU-WDE711 '    TO CURRENT-IMS-SECTION                      
268805                                                                          
268905                                                                          
269005     STRING 'WDE701  (WDE701KY =' W-WDE701KY ')'                          
269105          DELIMITED BY SIZE INTO SSA1                                     
269205     STRING 'WDE711  (IDKOLLIS =' W-IDKOLLI-SAMP-X ')'                    
269305          DELIMITED BY SIZE INTO SSA2                                     
269405                                                                          
269505     MOVE '  GE' TO GODK-STATUSKODER                                      
269605     CALL CBLTDLI USING GU WDE7-PCB DLI-IO-WDE711 SSA1 SSA2               
269705     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
269805     PERFORM IMS-STATUSKONTROLL                                           
269905     .                                                                    
270005     EJECT                                                                
270105 IMS-GHU-WDE711 SECTION.                                                  
270205                                                                          
270305     MOVE 'IMS-GHU-WDE711 '    TO CURRENT-IMS-SECTION                     
270405                                                                          
270505     STRING 'WDE701  (WDE701KY =' W-WDE701KY ')'                          
270605          DELIMITED BY SIZE INTO SSA1                                     
270705     STRING 'WDE711  (IDKOLLIS =' W-IDKOLLI-SAMP-X ')'                    
270805          DELIMITED BY SIZE INTO SSA2                                     
270905                                                                          
271005     MOVE '  GE' TO GODK-STATUSKODER                                      
271105     CALL CBLTDLI USING GHU WDE7-PCB DLI-IO-WDE711 SSA1 SSA2              
271205     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
271305     PERFORM IMS-STATUSKONTROLL                                           
271405     .                                                                    
271505                                                                          
271605                                                                          
271705 IMS-GNP-WDE711  SECTION.                                                 
271805     MOVE 'IMS-GNP-WDE711 '    TO CURRENT-IMS-SECTION                     
271905                                                                          
272005     MOVE '  GE'                TO GODK-STATUSKODER                       
272105     MOVE   'WDE711   '          TO SSA1                                  
272205                                                                          
272305     CALL CBLTDLI USING GNP  WDE7-PCB DLI-IO-WDE711 SSA1                  
272405     MOVE WDE7-STATUS-CODE      TO STATUS-WS                              
272505     PERFORM IMS-STATUSKONTROLL                                           
272605     .                                                                    
272705                                                                          
272805 IMS-REPL-WDE711 SECTION.                                                 
272905     MOVE 'IMS-REPL-WDE711 '    TO CURRENT-IMS-SECTION                    
273005                                                                          
273105                                                                          
273205     MOVE '  ' TO GODK-STATUSKODER                                        
273305     CALL CBLTDLI USING REPL WDE7-PCB DLI-IO-WDE711                       
273405     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
273505     PERFORM IMS-STATUSKONTROLL                                           
273605     .                                                                    
273705     EJECT                                                                
273706                                                                          
278205 IMS-DEL-WDE711 SECTION.                                                  
278305     MOVE 'IMS-DEL-WDE711 '    TO CURRENT-IMS-SECTION                     
278405                                                                          
278505     MOVE '  ' TO GODK-STATUSKODER                                        
278605     CALL CBLTDLI USING DLET WDE7-PCB DLI-IO-WDE711                       
278705     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
278805     PERFORM IMS-STATUSKONTROLL                                           
278905     .                                                                    
278906                                                                          
278907 IMS-ISRT-WDE711  SECTION.                                                
278908     MOVE 'IMS-ISRT-WDE711 '    TO CURRENT-IMS-SECTION                    
278909                                                                          
278910     STRING 'WDE701  (WDE701KY =' W-WDE701KY ')'                          
278911          DELIMITED BY SIZE INTO SSA1                                     
278912     MOVE   'WDE711 '           TO   SSA2                                 
278913     MOVE '  IINI'              TO GODK-STATUSKODER                       
278914                                                                          
278915     CALL CBLTDLI USING ISRT WDE7-PCB DLI-IO-WDE711 SSA1 SSA2             
278916     MOVE WDE7-STATUS-CODE      TO STATUS-WS                              
278917     PERFORM IMS-STATUSKONTROLL                                           
278918     .                                                                    
278919     EJECT                                                                
278920 IMS-GHU-WDE721 SECTION.                                                  
278921                                                                          
278922     MOVE 'IMS-GHU-WDE721 '    TO CURRENT-IMS-SECTION                     
278923                                                                          
278924     STRING 'WDE701  (WDE701KY =' W-WDE701KY ')'                          
278925          DELIMITED BY SIZE INTO SSA1                                     
278926     STRING 'WDE711  (IDKOLLIS =' W-IDKOLLI-SAMP-X ')'                    
278927          DELIMITED BY SIZE INTO SSA2                                     
278928     STRING 'WDE721  (WDE721KY =' W-WDE721KY ')'                          
278929          DELIMITED BY SIZE INTO SSA3                                     
278930                                                                          
278931     MOVE '  GE' TO GODK-STATUSKODER                                      
278932     CALL CBLTDLI USING GHU WDE7-PCB DLI-IO-WDE721 SSA1 SSA2 SSA3         
278933     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
278934     PERFORM IMS-STATUSKONTROLL                                           
278935     .                                                                    
278936 IMS-ISRT-WDE721  SECTION.                                                
278937     MOVE 'IMS-ISRT-WDE721 '    TO CURRENT-IMS-SECTION                    
278938                                                                          
278939     STRING 'WDE701  (WDE701KY =' W-WDE701KY ')'                          
278940          DELIMITED BY SIZE INTO SSA1                                     
278941     STRING 'WDE711  (IDKOLLIS =' W-IDKOLLI-SAMP-X ')'                    
278942          DELIMITED BY SIZE INTO SSA2                                     
278943     MOVE   'WDE721 '           TO   SSA3                                 
278944     MOVE '  IINI'              TO GODK-STATUSKODER                       
278945     CALL CBLTDLI USING ISRT WDE7-PCB DLI-IO-WDE721 SSA1 SSA2 SSA3        
278946     MOVE WDE7-STATUS-CODE      TO STATUS-WS                              
278947     PERFORM IMS-STATUSKONTROLL                                           
278948     .                                                                    
278949                                                                          
278950 IMS-DEL-WDE721 SECTION.                                                  
278951     MOVE 'IMS-DEL-WDE721 '    TO CURRENT-IMS-SECTION                     
278952                                                                          
278953     MOVE '  ' TO GODK-STATUSKODER                                        
278954     CALL CBLTDLI USING DLET WDE7-PCB DLI-IO-WDE721                       
278955     MOVE WDE7-STATUS-CODE TO STATUS-WS                                   
278956     PERFORM IMS-STATUSKONTROLL                                           
278957     .                                                                    
279005     EJECT                                                                
279105 IMS-GU-WDE711-ASEQ  SECTION.                                             
279205     MOVE 'IMS-GU-WDE711-ASEQ '    TO CURRENT-IMS-SECTION                 
279305                                                                          
279405     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
279505          DELIMITED BY SIZE INTO SSA1                                     
279605                                                                          
279705     MOVE '  GE' TO GODK-STATUSKODER                                      
279805     CALL CBLTDLI USING GU WDE7A-PCB DLI-IO-WDE711 SSA1                   
279905     MOVE WDE7A-STATUS-CODE TO STATUS-WS                                  
280005     PERFORM IMS-STATUSKONTROLL                                           
280105     .                                                                    
280205     EJECT                                                                
280305                                                                          
280405 IMS-GHU-WDE711-ASEQ  SECTION.                                            
280505     MOVE 'IMS-GHU-WDE711-ASEQ '    TO CURRENT-IMS-SECTION                
280605                                                                          
280705     STRING 'WDE711  (WDE7ASEQ =' W-WDE7ASEQ-X ')'                        
280805          DELIMITED BY SIZE INTO SSA1                                     
280905     MOVE '  GE' TO GODK-STATUSKODER                                      
281005     CALL CBLTDLI USING GHU WDE7A-PCB DLI-IO-WDE711 SSA1                  
281105     MOVE WDE7A-STATUS-CODE TO STATUS-WS                                  
281205     PERFORM IMS-STATUSKONTROLL                                           
281305     .                                                                    
281405                                                                          
281505 IMS-REPL-WDE711-ASEQ SECTION.                                            
281605     MOVE 'IMS-REPL-WDE711 '    TO CURRENT-IMS-SECTION                    
281705                                                                          
281805     MOVE '  ' TO GODK-STATUSKODER                                        
281905     CALL CBLTDLI USING REPL WDE7A-PCB DLI-IO-WDE711                      
282005     MOVE WDE7A-STATUS-CODE TO STATUS-WS                                  
282105     PERFORM IMS-STATUSKONTROLL                                           
282205     .                                                                    
282305     EJECT                                                                
282405                                                                          
282505 IMS-GNP-WDE701-ASEQ SECTION.                                             
282605     MOVE 'IMS-GNP-WDE701-ASEQ'    TO CURRENT-IMS-SECTION                 
282705                                                                          
282805     MOVE   'WDE701   '          TO SSA1                                  
282905     MOVE '  GE'                TO GODK-STATUSKODER                       
283005     CALL CBLTDLI USING GNP  WDE7A-PCB DLI-IO-WDE701 SSA1                 
283105     MOVE WDE7A-STATUS-CODE      TO STATUS-WS                             
283205     PERFORM IMS-STATUSKONTROLL                                           
283305     .                                                                    
283405                                                                          
283505 IMS-GNP-WDE721-ASEQ SECTION.                                             
283605     MOVE 'IMS-GNP-WDE721-ASEQ'    TO CURRENT-IMS-SECTION                 
283705                                                                          
283805     MOVE   'WDE721   '          TO SSA1                                  
283905     MOVE '  GE'                TO GODK-STATUSKODER                       
284005     CALL CBLTDLI USING GNP  WDE7A-PCB DLI-IO-WDE721 SSA1                 
284105     MOVE WDE7A-STATUS-CODE      TO STATUS-WS                             
284205     PERFORM IMS-STATUSKONTROLL                                           
284305     .                                                                    
284405                                                                          
284505     EJECT                                                                
284605 IMS-GU-WDE601 SECTION.                                                   
284705     MOVE 'IMS-GU-WDE601      '    TO CURRENT-IMS-SECTION                 
284805                                                                          
284905     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
285005          DELIMITED BY SIZE INTO SSA1                                     
285105     MOVE '  GE' TO GODK-STATUSKODER                                      
285205     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
285305     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
285405     PERFORM IMS-STATUSKONTROLL                                           
285505     .                                                                    
285605     EJECT                                                                
285705 IMS-GNP-WDE611 SECTION.                                                  
285805     MOVE 'IMS-GNP-WDE611      '    TO CURRENT-IMS-SECTION                
285905                                                                          
286005     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
286105          DELIMITED BY SIZE INTO SSA1                                     
286205     MOVE '  GE' TO GODK-STATUSKODER                                      
286305     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1                   
286405     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
286505     PERFORM IMS-STATUSKONTROLL                                           
286605     .                                                                    
286705     EJECT                                                                
286805 IMS-GU-WDE611 SECTION.                                                   
286905     MOVE 'IMS-GU-WDE611      '    TO CURRENT-IMS-SECTION                 
287005                                                                          
287105     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
287205                      DELIMITED BY SIZE INTO SSA1                         
287305     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
287405                      DELIMITED BY SIZE INTO SSA2                         
287505     MOVE '  GE' TO GODK-STATUSKODER                                      
287605     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
287705     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
287805     PERFORM IMS-STATUSKONTROLL                                           
287905     .                                                                    
288005     EJECT                                                                
288105 IMS-GHU-WDE611 SECTION.                                                  
288205     MOVE 'IMS-GHU-WDE611      '    TO CURRENT-IMS-SECTION                
288305                                                                          
288405     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
288505                      DELIMITED BY SIZE INTO SSA1                         
288605     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
288705                      DELIMITED BY SIZE INTO SSA2                         
288805     MOVE '  GE' TO GODK-STATUSKODER                                      
288905     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
289005     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
289105     PERFORM IMS-STATUSKONTROLL                                           
289205     .                                                                    
289305                                                                          
289405 IMS-REPL-WDE611 SECTION.                                                 
289505     MOVE 'IMS-REPL-WDE611      '    TO CURRENT-IMS-SECTION               
289605                                                                          
289705     MOVE '  ' TO GODK-STATUSKODER                                        
289805     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
289905     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
290005     PERFORM IMS-STATUSKONTROLL                                           
290105     .                                                                    
290205     EJECT                                                                
290305 IMS-GU-WDE401-ASEQ SECTION.                                              
290405     MOVE 'IMS-GU-WDE401-ASEQ '    TO CURRENT-IMS-SECTION                 
290505                                                                          
290605     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
290705          DELIMITED BY SIZE INTO SSA1                                     
290805     MOVE '  GE' TO GODK-STATUSKODER                                      
290905     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
291005     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
291105     PERFORM IMS-STATUSKONTROLL                                           
291205     .                                                                    
291305     EJECT                                                                
291405 IMS-GN-WDE401-ASEQ            SECTION.                                   
291505     MOVE 'IMS-GN-WDE401-ASEQ '    TO CURRENT-IMS-SECTION                 
291605                                                                          
291705     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
291805     DELIMITED BY SIZE INTO SSA1                                          
291905     MOVE '  GEGB' TO GODK-STATUSKODER                                    
292005     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-WDE401 SSA1                    
292105     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
292205     PERFORM IMS-STATUSKONTROLL                                           
292305     .                                                                    
292405     EJECT                                                                
292505                                                                          
292605 IMS-GU-WDK501 SECTION.                                                   
292705     MOVE 'IMS-GU-WDK501      '    TO CURRENT-IMS-SECTION                 
292805                                                                          
292905     STRING 'WDK501  (KDKOLLI  =' W-KDKOLLI-K5-X ')'                      
293005          DELIMITED BY SIZE INTO SSA1                                     
293105     MOVE '  GE'             TO GODK-STATUSKODER                          
293205     CALL CBLTDLI USING GU WDK5-PCB DLI-IO-WDK501 SSA1                    
293305     MOVE WDK5-STATUS-CODE   TO STATUS-WS                                 
293405     PERFORM IMS-STATUSKONTROLL                                           
293505     .                                                                    
293605 IMS-GU-WDGX4539-40       SECTION.                                        
293705     MOVE 'IMS-GU-WDGX4539-40'    TO CURRENT-IMS-SECTION                  
293805                                                                          
293905     MOVE SPACE TO SSA1                                                   
294005                   SSA2                                                   
294105                                                                          
294205     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4539-X ')'                    
294305         DELIMITED BY SIZE INTO SSA1                                      
294405     STRING 'WDGX4540(KDSEGKEY =' W-WDGXKEY-4540-X ')'                    
294505         DELIMITED BY SIZE INTO SSA2                                      
294605                                                                          
294705     MOVE '  GE' TO GODK-STATUSKODER                                      
294805     CALL CBLTDLI USING GU 4539-PCB 4540-WDGX4540 SSA1 SSA2               
294905     MOVE 4539-STATUS-CODE TO STATUS-WS                                   
295005                                                                          
295105     PERFORM IMS-STATUSKONTROLL                                           
295205     .                                                                    
295305 IMS-GHU-WDGX4539-40       SECTION.                                       
295405     MOVE 'IMS-GHU-WDGX4539-40'    TO CURRENT-IMS-SECTION                 
295505                                                                          
295605     MOVE SPACE TO SSA1                                                   
295705                   SSA2                                                   
295805                                                                          
295905     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4539-X ')'                    
296005         DELIMITED BY SIZE INTO SSA1                                      
296105     STRING 'WDGX4540(KDSEGKEY =' W-WDGXKEY-4540-X ')'                    
296205         DELIMITED BY SIZE INTO SSA2                                      
296305                                                                          
296405     MOVE '  GE' TO GODK-STATUSKODER                                      
296505     CALL CBLTDLI USING GHU 4539-PCB 4540-WDGX4540 SSA1 SSA2              
296605     MOVE 4539-STATUS-CODE TO STATUS-WS                                   
296705                                                                          
296805     PERFORM IMS-STATUSKONTROLL                                           
296905     .                                                                    
297005     SKIP3                                                                
297105                                                                          
297205 IMS-REPL-WDGX4540        SECTION.                                        
297305     MOVE 'IMS-REPL-WDGX4540  '    TO CURRENT-IMS-SECTION                 
297405                                                                          
297505     MOVE '    ' TO GODK-STATUSKODER                                      
297605     CALL CBLTDLI USING REPL 4539-PCB 4540-WDGX4540                       
297705     MOVE 4539-STATUS-CODE TO STATUS-WS                                   
297805                                                                          
297905     PERFORM IMS-STATUSKONTROLL                                           
298005     .                                                                    
298105     SKIP3                                                                
298205 IMS-STATUSKONTROLL SECTION.                                              
298305                                                                          
298405     SET STATUS-IX TO 1                                                   
298505     SEARCH GODK-STATUS                                                   
298605       AT END                                                             
298705         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
298805         DELIMITED BY SIZE INTO FELTEXT                                   
298905         CALL FELLOG                                                      
299005       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
299105         CONTINUE                                                         
299205     END-SEARCH                                                           
300002     .                                                                    
