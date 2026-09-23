000100 ID DIVISION.                                                             
000201     SKIP2                                                                
000301 PROGRAM-ID.     W6013610.                                                
000401*AUTHOR.         ANNELIE ENGLUND / ARCHANA BHAT.                          
000501*DATE-WRITTEN.   92/08/07 / JUNE 2012.                                    
000601                                                                          
000701*    REMARKS.                                                             
000801*                                                                         
000901*    FUNKTION:                                                            
001001*        PROGRAMMET ÄR EN MPP SOM ANVÄNDS NÄR MAN VILL FÖRPACKA           
001101*        ETT PARTI, ENSKILT KOLLI ELLER VID OMPACKNING.                   
001201*        FÖRBEHANDLINGSRAPPORT KAN BESTÄLLAS FRÅN BILDEN.                 
001301*        NÄR FÖRPACKNING/OMPACKNING ÄR UTFÖRD, BESTÄLLS FLAGGOR           
001401*        OCH ETIKETTER UT.                                                
001501*                                                                         
001601*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001701*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001801*        PROGRAMMET LÄSER      WDK6                                       
001901*        PROGRAMMET LÄSER      WDB6                                       
002001*        PROGRAMMET UPPDATERAR W6LOPA (W6G1)                              
002101*                                                                         
002201*    INDATA.                                                              
002401*        REQU:  W60136I1                                                  
002501*                                                                         
002601*    UTDATA.                                                              
002701*        RESP:  W60136O1                                                  
002801                                                                          
002901     SKIP3                                                                
003001 ENVIRONMENT DIVISION.                                                    
003101     EJECT                                                                
003201 DATA DIVISION.                                                           
003301 WORKING-STORAGE SECTION.                                                 
003401                                                                          
003501*    -- CHECKED BY WY2000                                                 
003601 77  IDPGM                       PIC X(08)   VALUE 'W6013610'.            
003701                                                                          
003801*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003901 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004001                                                                          
004101 77  JA                          PIC X       VALUE 'J'.                   
004201 77  YES                         PIC X       VALUE 'Y'.                   
004301 77  NEJ                         PIC X       VALUE 'N'.                   
004401 77  PRIO                        PIC X       VALUE 'P'.                   
004402                                                                          
004403*01 -COPY WWDC99                                                          
004501                                                                          
004601 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004801*   OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                   
005001 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005101                                                                          
005201*    --- GENERELLA ARBETSFÄLT ---                                         
005301                                                                          
005401 77  WS-KDMFSFOR                 PIC X       VALUE SPACE.                 
005501     88  WS-SWEDISH-TEXT                     VALUE '1'.                   
005601     88  WS-ENGLISH-TEXT                     VALUE '2'.                   
005701*    --- RÄKNARE                                                          
005801 77  WS-KVAVIS-RAKN              PIC S9(6)   VALUE ZERO.                  
005901 77  WS-FPK-RAKN                 PIC S9(6)   VALUE ZERO.                  
006001 77  WS-PRIO-RAKN                PIC S9(6)   VALUE ZERO.                  
006101 77  WS-KIT-RAKN                 PIC S9(6)   VALUE ZERO.                  
006201 77  KVFLETI-RAKN                PIC S9(2)   VALUE ZERO.                  
006301                                                                          
006401*    --- BERÄKNINGSFÄLT                                                   
006501 77  WS-KVAVIS-VERKL             PIC S9(6)   VALUE ZERO.                  
006601 77  WS-KVAVIS-FPK-KVAR          PIC S9(6)   VALUE ZERO.                  
006701 77  WS-KVAVIS-PRIO-KVAR         PIC S9(6)   VALUE ZERO.                  
006801 77  WS-KVAVIS-KIT-KVAR          PIC S9(6)   VALUE ZERO.                  
006901 77  WS-KVAVIS-MOT               PIC S9(6)   VALUE ZERO.                  
007001 77  WS-KVINLART-SEK             PIC S9(6)   VALUE ZERO.                  
007101 77  WS-SUMMA-ART-BILD           PIC S9(6)   VALUE ZERO.                  
007201 77  WS-RAD1-KVINLART            PIC S9(6)   VALUE ZERO.                  
007301 77  WS-DIFFERENS                PIC S9(6)   VALUE ZERO.                  
007401 77  WS-KVAANT                   PIC S9(6)   VALUE ZERO.                  
007501 77  WS-KVFLETI-TOT              PIC S9(3)   VALUE ZERO.                  
007601                                                                          
007701*    --- TRANSFÄLT                                                        
007801 77  WS-TRANS-ADINLOMR-OLD       PIC X(4)    VALUE SPACE.                 
007901 77  WS-TRANS-ADINLOMR-NXT-OLD   PIC X(4)    VALUE SPACE.                 
008001 77  WS-TRANS-KDINLSTA-OLD       PIC X(3)    VALUE SPACE.                 
008101 77  WS-TRANS-KVINLART-OLD       PIC 9(6)    VALUE ZERO.                  
008201                                                                          
008301*    --- SPARFÄLT                                                         
008401 77  W-ADINLOMR-PRT              PIC X(4)    VALUE SPACE.                 
008501 77  SPAR-ADINLOMR               PIC X(4)    VALUE SPACE.                 
008601 77  SPAR-FLPRIO                 PIC X       VALUE SPACE.                 
008701 77  SPAR-FLINLFB                PIC X       VALUE SPACE.                 
008801 77  SPAR-FLKVAANT               PIC X       VALUE SPACE.                 
008901 77  SPAR-IDLEVNR                PIC X(5)    VALUE SPACE.                 
009001 77  SPAR-KDINLPRIO              PIC 9(2)    VALUE ZERO.                  
009101 77  SPAR-IDPRTLST-VAL           PIC X(2)    VALUE SPACE.                 
009201                                                                          
009301*    --- INDEX                                                            
009401 77  INDX                        PIC  S9(2)  VALUE ZERO.                  
009501 77  6191-IX                     PIC  S9(2)  VALUE ZERO.                  
009601 77  6194-IX                     PIC  S9(2)  VALUE ZERO.                  
009701 77  6195-IX                     PIC  S9(2)  VALUE ZERO.                  
009801 77  6197-IX                     PIC  S9(2)  VALUE ZERO.                  
009901                                                                          
010001*    --- KONSTANTER                                                       
010101 77  FLAGGA                      PIC X(2)    VALUE 'FL'.                  
010201*    BORDE HETA LABEL MEN LABEL ÄR ETT RESERVERAT ORD AV COBOL            
010301 77  LAB                         PIC X(2)    VALUE 'LA'.                  
010401 77  ETIKETT                     PIC X(2)    VALUE 'ET'.                  
010501 77  MLABEL                      PIC X(2)    VALUE 'ML'.                  
010601 77  W-KVKRBEH                   PIC X(4)    VALUE '00.5'.                
010701                                                                          
010801*    --- TABELL                                                           
010901 01  INDATARADER.                                                         
011001     03  RADINFO  OCCURS 6.                                               
011101         05  WS-KVFLETI          PIC 9(2)    VALUE ZERO.                  
011201         05  WS-KVINLART         PIC 9(6)    VALUE ZERO.                  
011301                                                                          
011401                                                                          
011501*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
011601 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
012401*                                                                         
012501       EJECT                                                              
012601*    --- SWITCHAR                                                         
012701 77  INDATA-SW                   PIC X       VALUE 'J'.                   
012801     88  INDATA-OK                           VALUE 'J'.                   
012901     88  INDATA-FEL                          VALUE 'N'.                   
013001                                                                          
013101 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013201     88  NYCKLAR-OK                          VALUE 'J'.                   
013301     88  NYCKLAR-FEL                         VALUE 'N'.                   
013401                                                                          
014401 77  INPUT-SW                    PIC X       VALUE 'N'.                   
014501     88  INPUT-FINNS                         VALUE 'J'.                   
014601     88  INGEN-INPUT                         VALUE 'N'.                   
014701                                                                          
014801 77  RADER-SW                    PIC X       VALUE 'N'.                   
014901     88  RADER-IFYLLDA                       VALUE 'J'.                   
015001     88  RADER-EJ-IFYLLDA                    VALUE 'N'.                   
015101                                                                          
015201 77  AVVIK-SW                    PIC X       VALUE 'N'.                   
015301     88  AVVIK-IFYLLD                        VALUE 'J'.                   
015401     88  AVVIK-EJ-IFYLLD                     VALUE 'N'.                   
015501                                                                          
015601 77  TRAFF-SW                    PIC X       VALUE 'N'.                   
015701     88  TRAFF                               VALUE 'J'.                   
015801                                                                          
015901 77  PRIO-SW                     PIC X       VALUE 'N'.                   
016001     88  PRIOMRK                             VALUE 'J'.                   
016101     88  EJ-PRIO                             VALUE 'N'.                   
016201                                                                          
016301 77  SATS-SW                     PIC X       VALUE 'N'.                   
016401     88  SATS                                VALUE 'J'.                   
016501     88  EJ-SATS                             VALUE 'N'.                   
016601                                                                          
016701 77  KOLLI-KVAR-SW               PIC X       VALUE 'N'.                   
016801     88  KOLLI-KVAR                          VALUE 'J'.                   
016901                                                                          
017001 77  FOERSTA-SW                  PIC X       VALUE 'J'.                   
017101     88  FOERSTA-IFYLLD                      VALUE 'J'.                   
017201                                                                          
017301 77  OMPACKNINGS-SW              PIC X       VALUE 'N'.                   
017401     88  OMPACKNING                          VALUE 'J'.                   
017501                                                                          
017601 77  ANTALSKNTR-SW               PIC X       VALUE 'J'.                   
017701     88  ANTALSKNTR-KLAR                     VALUE 'J'.                   
017801                                                                          
017901 77  FLAGG-SW                    PIC X       VALUE 'N'.                   
018001     88  FLAGGA-VALD                         VALUE 'J'.                   
018101                                                                          
018201 77  ETIKETT-SW                  PIC X       VALUE 'N'.                   
018301     88  ETIKETT-VALD                        VALUE 'J'.                   
018401                                                                          
018501 77  FPK-SW                      PIC X       VALUE 'J'.                   
018601     88  FPK-PARTI                           VALUE 'J'.                   
018701                                                                          
018801 77  PARTI-SW                    PIC X       VALUE 'N'.                   
018901     88  PARTI-FEL                           VALUE 'J'.                   
019001                                                                          
019101 77  6191-SW                     PIC X       VALUE 'J'.                   
019201     88  FOERSTA-6191                        VALUE 'J'.                   
019301                                                                          
019401 77  6193-SW                     PIC X       VALUE 'J'.                   
019501     88  6193-TRANS                          VALUE 'J'.                   
019601                                                                          
019701 77  6194-SW                     PIC X       VALUE 'J'.                   
019801     88  FOERSTA-6194                        VALUE 'J'.                   
019901                                                                          
020001 77  6195-SW                     PIC X       VALUE 'J'.                   
020101     88  FOERSTA-6195                        VALUE 'J'.                   
020201                                                                          
020301 77  R6202-SW                    PIC X       VALUE 'N'.                   
020401     88  R6202-TRANS                         VALUE 'J'.                   
020501                                                                          
020601 77  PRIM-CONTROL-SW             PIC X       VALUE 'N'.                   
020701     88  PRIM-CONTROL-YES                    VALUE 'J'.                   
020801     88  PRIM-CONTROL-NO                     VALUE 'N'.                   
020901                                                                          
021001     EJECT                                                                
021101*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021201 01  GENERELLA-SUBPROGRAM.                                                
021401     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021501     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021601     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
021701     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
021801     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
021901     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
022101     EJECT                                                                
022201*01 -COPY WMSGINIT                                                        
022301     SKIP3                                                                
022701 01  MESSAGE-CODES.                                                       
022801     03  ERR-CORR-HILITE-FLDS     PIC X(3)    VALUE '020'.                
023001     03  ERR-NOT-VALID            PIC X(3)    VALUE '007'.                
023101     03  ERR-MISSING              PIC X(3)    VALUE '025'.                
023201     03  ERR-PF11-AND-NO-DATA     PIC X(3)    VALUE '014'.                
023501     03  ERR-ON-CASELEVEL         PIC X(3)    VALUE '367'.                
023601     03  ERR-WRONG-NUMBER         PIC X(3)    VALUE '330'.                
023701     03  ERR-DIV-CASE             PIC X(3)    VALUE '354'.                
023801     03  ERR-KIT-PRIO             PIC X(3)    VALUE '370'.                
023901     03  ERR-KIT-CASE             PIC X(3)    VALUE '334'.                
024001     03  ERR-UNPACKED-CASE        PIC X(3)    VALUE '178'.                
024101     03  ERR-DEVIATION-6133       PIC X(3)    VALUE '382'.                
024201     03  ERR-FLAG-LABEL-TYPE      PIC X(3)    VALUE '383'.                
024301     03  ERR-TOO-MANY             PIC X(3)    VALUE '384'.                
024401     03  ERR-WRONG-KEY            PIC X(3)    VALUE '022'.                
024501     03  ERR-PRT-MISSING          PIC X(3)    VALUE '347'.                
024601     03  ERR-NOT-3-OR-77          PIC X(3)    VALUE '373'.                
024701     03  ERR-MESSAGE-MISSING      PIC X(3)    VALUE '999'.                
024801     03  SPLITT-PRESS-PF23        PIC X(3)    VALUE '353'.                
024901     03  ERR-PRI-SPL-CTL-NOT-DONE PIC X(3)    VALUE '377'.                
025001                                                                          
025101     03  INF-UPDATE-DONE          PIC X(3)    VALUE '001'.                
025201     03  INF-PRESS-PF11           PIC X(3)    VALUE '013'.                
025301     EJECT                                                                
025401*01  -COPY W611PMRK                                                       
025501     EJECT                                                                
025601*01  -COPY W611STYR                                                       
025701     EJECT                                                                
025801*01  -COPY W006PRT                                                        
025901     EJECT                                                                
026001*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
026101*                                                                         
027301 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
027401     SKIP3                                                                
027501*01  -COPY WMFSAREA                                                       
027601     EJECT                                                                
027701 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
027801     SKIP3                                                                
027901*01  -COPY WMSGKOM                                                        
028001     EJECT                                                                
028101 01  FILLER                      PIC X(16)  VALUE 'MSG/KOM-AREA'.         
028201     SKIP3                                                                
028301*01  -COPY WMSGSNUF   -PRE  P-TO-P-                                       
028401     EJECT                                                                
028501*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028601*                                                                         
028701 01  ALL-SPACE.                                                           
028801     03 FILLER                   PIC X(80)   VALUE SPACE.                 
028901 01  ALL-PLUS.                                                            
029001     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
029101     EJECT                                                                
029201 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
029301     SKIP3                                                                
029401 01  NYCKLAR-TILL-DLI.                                                    
029501*----W6D1                                                                 
029601     03  W-IDDC-X.                                                        
029701         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
029801     03  W-IDRADNR-INL-X.                                                 
029901         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
030001     03  W-IDRADNR-X.                                                     
030101         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
030201     03  W-W6D1BSEQ-X.                                                    
030301         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
030401     03  W1-IDLOPNRM-X.                                                   
030501         05  W1-IDLOPNRM          PIC S9(9)   COMP-3.                     
030601     03  W-W6D101KY-X.                                                    
030701         05  W-IDDC-101KY        PIC  X(2)   VALUE SPACE.                 
030801         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
030901         05  W-IDFS              PIC X(8)    VALUE LOW-VALUE.             
031001         05  W-TIAVIDAT          PIC S9(7)   VALUE ZERO COMP-3.           
031101*----W6G1                                                                 
031201     03  W-W6GXKEY-6005-X.                                                
031301         05  W-IDHTYP-6005       PIC X(4)    VALUE '6005'.                
031401         05  W-IDDC-6005         PIC X(2)    VALUE SPACE.                 
031501         05  W-VALFRI-6005       PIC X(24)   VALUE LOW-VALUE.             
031601     03  W-W6GXKEY-6006-X.                                                
031701         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
031801         05  W-VALFRI-6006       PIC X(1)    VALUE LOW-VALUE.             
031901*----W6G1/WDK6                                                            
032001     03  W-W6GXKEY-6017-X.                                                
032101         05  W-IDHTYP-6017       PIC X(4)     VALUE '6017'.               
032201         05  W-VALFRI-6017       PIC X(26)    VALUE LOW-VALUE.            
032301     03  W-IDARTNR-X.                                                     
032401         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
032501     03  W-KDSEGKEY-X.                                                    
032601         05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                  
032701*----W6H7 KVALITE                                                         
032801     03  W-W6H7CSEQ-X.                                                    
032901         05  W-IDLOPNRM-H7       PIC S9(9)    VALUE ZERO COMP-3.          
033001         05  W-DAAVSDAT-H7       PIC  9(8)    VALUE ZERO.                 
033101     03  W-W6H7BSEQ-MIN-X.                                                
033201         05  W-IDARTNR-H7-MIN    PIC S9(9)    VALUE ZERO COMP-3.          
033301         05  W-DAREGDAT-9KOMPL-MIN                                        
033401                                 PIC 9(8)     VALUE ZERO.                 
033501         05  W-IDLEVNR-H7-MIN-X.                                          
033601           07  W-IDLEVNR-H7-MIN  PIC X(5)     VALUE SPACE.                
033701         05  W-KVKRKNTR-H7-MIN-X.                                         
033801           07  W-KVKRKNTR-H7-MIN PIC S9       VALUE ZERO COMP-3.          
033901     03  W-W6H7BSEQ-MAX-X.                                                
034001         05  W-IDARTNR-H7-MAX    PIC S9(9)    VALUE ZERO COMP-3.          
034101         05  W-DAREGDAT-9KOMPL-MAX                                        
034201                                 PIC 9(8)     VALUE ZERO.                 
034301         05  W-IDLEVNR-H7-MAX-X.                                          
034401           07  W-IDLEVNR-H7-MAX  PIC X(5)     VALUE SPACE.                
034501         05  W-KVKRKNTR-H7-MAX-X.                                         
034601           07  W-KVKRKNTR-H7-MAX PIC S9       VALUE ZERO COMP-3.          
034701*----WDB6 DC                                                              
034801     03  W-IDDC-B6-X.                                                     
034901         05 W-IDDC-B6            PIC X(2).                                
035001                                                                          
035002     03  W-IDLEVNR-CN-US.                                                 
035003         05 W-IDLEVNR-X3         PIC X(3) VALUE '990'.                    
035004         05 W-IDDC-CN-US         PIC X(2) VALUE SPACE.                    
035101     SKIP2                                                                
035201*    --- STATUS-KOD FRÅN IMS                                              
035301 01  STATUS-WS                   PIC XX.                                  
035401     88  SEGMENT-FINNS                       VALUE '  '.                  
035501     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
035601     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
035701     SKIP2                                                                
035801 01  GODK-STATUSKODER.                                                    
035901     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036001     SKIP3                                                                
036101 01  SSA1                        PIC X(128).                              
036201 01  SSA2                        PIC X(64).                               
036301     EJECT                                                                
036401*    --- IMS FUNKTIONSKODER                                               
036501*01  -COPY W0003                                                          
036601     EJECT                                                                
036701*    ---  DLI ARBETSAREA                                                  
036801 01  FILLER                      PIC X(16)   VALUE 'DLI-WS-AREA'.         
036901     SKIP3                                                                
037001 01  DLI-WS-AREA.                                                         
037101     03  WS-AREA                 PIC X(150)  VALUE SPACE.                 
037201     SKIP3                                                                
037301     03  W6INLA11 REDEFINES WS-AREA.                                      
037401*        05  -COPY W6D111  -PRE WS-INLA-                                  
037501     EJECT                                                                
037601*    ---  DLI INPUT-OUTPUT AREA                                           
037701 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
037801     SKIP3                                                                
037901 01  DLI-IO-AREA.                                                         
038001     03  IO-AREA                 PIC X(550)  VALUE SPACE.                 
038101     SKIP3                                                                
038201     03  W6INLA11 REDEFINES IO-AREA.                                      
038301*        05  -COPY W6D111  -PRE INLA-                                     
038401     SKIP3                                                                
038501     03  W6INLA21 REDEFINES IO-AREA.                                      
038601*        05  -COPY W6D121  -PRE INLA-                                     
038701     SKIP3                                                                
038801     03  W6PLAA01 REDEFINES IO-AREA.                                      
038901*        05  -COPY W6GX01  -PRE PLAA-                                     
039001     SKIP3                                                                
039101     03  W6PLAA11 REDEFINES IO-AREA.                                      
039201*        05  -COPY W6GX6006 -PRE PLAA-                                    
039301     SKIP3                                                                
039401     03  W6KVAE01 REDEFINES IO-AREA.                                      
039501*        05  -COPY W6H701 -PRE KVAE-                                      
039601     SKIP3                                                                
039701 01  DLI-INLA-AREA.                                                       
039801     03  INLA-AREA               PIC X(150)  VALUE SPACE.                 
039901     SKIP3                                                                
040001     03  W6INLA01 REDEFINES INLA-AREA.                                    
040101*        05  -COPY W6D101  -PRE INLA-                                     
040201     SKIP3                                                                
040301 01  DLI-INLC-AREA.                                                       
040401     03  INLC-AREA               PIC X(150)  VALUE SPACE.                 
040501     SKIP3                                                                
040601     03  W6INLC01 REDEFINES INLC-AREA.                                    
040701*        05  -COPY W6D1B1  -PRE INLC-                                     
040801     SKIP3                                                                
040901 01  DLI-LOPA-AREA.                                                       
041001     03  LOPA-AREA               PIC X(150)  VALUE SPACE.                 
041101     SKIP3                                                                
041201     03  W6LOPA01 REDEFINES LOPA-AREA.                                    
041301*        05  -COPY W6GX01  -PRE LOPA-                                     
041401     SKIP3                                                                
041501     03  W6LOPA11 REDEFINES LOPA-AREA.                                    
041601*        05  -COPY W6GX6018 -PRE LOPA-                                    
041701     EJECT                                                                
041801 01  DLI-IO-AREA-WDK6.                                                    
041901     03  WDK611.                                                          
042001*        05  -COPY WDK611                                                 
042101                                                                          
042201 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
042301 01   DLI-IO-AREA-B601.                                                   
042401*     03  -COPY WDB601                                                    
042501                                                                          
042601     EJECT                                                                
042701*    ---  TRANS AREA 6191                                                 
042801 01  FILLER                      PIC X(16)   VALUE 'MID-6191'.            
042901     SKIP3                                                                
043001 01  MID-6191-AREA.                                                       
043101     03  6191-AREA               PIC X(1600)  VALUE SPACE.                
043201     SKIP3                                                                
043301     03  W6I19101 REDEFINES 6191-AREA.                                    
043401*        05  -COPY W6I19101 -PRE 6191-                                    
043501     EJECT                                                                
043601*    ---  TRANS AREA 6193                                                 
043701 01  FILLER                      PIC X(16)   VALUE 'MID-6193'.            
043801     SKIP3                                                                
043901 01  MID-6193-AREA.                                                       
044001     03  6193-AREA               PIC X(1500)  VALUE SPACE.                
044101     SKIP3                                                                
044201     03  W6I19301 REDEFINES 6193-AREA.                                    
044301*        05  -COPY W6I19301 -PRE 6193-                                    
044401     EJECT                                                                
044501*    ---  TRANS AREA 6194                                                 
044601 01  FILLER                      PIC X(16)   VALUE 'MID-6194'.            
044701     SKIP3                                                                
044801 01  MID-6194-AREA.                                                       
044901     03  6194-AREA               PIC X(1500)  VALUE SPACE.                
045001     SKIP3                                                                
045101     03  W6I19401 REDEFINES 6194-AREA.                                    
045201*        05  -COPY W6I19401 -PRE 6194-                                    
045301     EJECT                                                                
045401*    ---  TRANS AREA 6195                                                 
045501 01  FILLER                      PIC X(16)   VALUE 'MID-6195'.            
045601     SKIP3                                                                
045701 01  MID-6195-AREA.                                                       
045801     03  6195-AREA               PIC X(1500)  VALUE SPACE.                
045901     SKIP3                                                                
046001     03  W6I19501 REDEFINES 6195-AREA.                                    
046101*        05  -COPY W6I19501 -PRE 6195-                                    
046201     EJECT                                                                
046301*    ---  TRANS AREA 6197                                                 
046401 01  FILLER                      PIC X(16)   VALUE 'MID-6197'.            
046501     SKIP3                                                                
046601 01  MID-6197-AREA.                                                       
046701     03  6197-AREA               PIC X(1500)  VALUE SPACE.                
046801     SKIP3                                                                
046901     03  W6I19701 REDEFINES 6197-AREA.                                    
047001*        05  -COPY W6I19701 -PRE 6197-                                    
047101     EJECT                                                                
047201*    ---  TRANS AREA 6202                                                 
047301 01  FILLER                      PIC X(16)   VALUE 'MID-6202'.            
047401     SKIP3                                                                
047402 01  FILLER                      PIC X(24)   VALUE                        
047403                                 '6202-MID-W60202I1'.                     
047404 01  6202-AREA.                                                           
047405     SKIP2                                                                
047406     03 -COPY WREQUPRE -PRE 6202-                                         
047407     03 -COPY WZ01REQU -PRE 6202-                                         
047408     03 -COPY W60202I1 -PRE 6202-                                         
047409     EJECT                                                                
047410                                                                          
048101 01  DLI-IO-AREA-UPFA01.                                                  
048201     03  W6UPFA01.                                                        
048301*        05  -COPY W6L101                                                 
048401     SKIP3                                                                
048501 01  DLI-IO-AREA-UPFA11.                                                  
048601     03  W6UPFA11.                                                        
048701*        05  -COPY W6L111                                                 
048801     SKIP3                                                                
048901 01  DLI-IO-AREA-UPFA12.                                                  
049001     03  W6UPFA12.                                                        
049101*        05  -COPY W6L112                                                 
049201     SKIP3                                                                
049301                                                                          
049401 LINKAGE SECTION.                                                         
049501                                                                          
049601 01  REQU-AREA.                                                           
049701*    03 -COPY WZ01REQU                                                    
049801*    03 -COPY W60136I1                                                    
049901     EJECT                                                                
050001 01  RESP-AREA.                                                           
050101*    03 -COPY WZ01RESP                                                    
050201*    03 -COPY W60136O1                                                    
050301     EJECT                                                                
050401 01  MAX-KVRADER                 PIC S9(4) COMP.                          
050501                                                                          
050601*01  -COPY W0009   -PRE MSG-                                              
050701     EJECT                                                                
050801*01  -COPY W0009   -PRE 6191-                                             
050901     EJECT                                                                
051001*01  -COPY W0009   -PRE 6194-                                             
051101     EJECT                                                                
051201*01  -COPY W0009   -PRE 6195-                                             
051301     EJECT                                                                
051401*01  -COPY W0009   -PRE 6197-                                             
051501     EJECT                                                                
051601*01  -COPY W0009   -PRE 6202-                                             
051701     EJECT                                                                
051801*01  -COPY W0009   -PRE DISP-                                             
051901     EJECT                                                                
052001*01  -COPY W0008  -PRE USEA-                                              
052101     05  FILLER                  PIC X.                                   
052201     EJECT                                                                
052301*01  -COPY W0008  -PRE INLA-                                              
052401     05  FILLER                  PIC X.                                   
052501     EJECT                                                                
052601*01  -COPY W0008  -PRE INLABSEQ-                                          
052701     05  FILLER                  PIC X.                                   
052801     EJECT                                                                
052901*01  -COPY W0008  -PRE INLC-                                              
053001     05  FILLER                  PIC X.                                   
053101     EJECT                                                                
053201*01  -COPY W0008  -PRE PLAA-                                              
053301     05  FILLER                  PIC X.                                   
053401     EJECT                                                                
053501*01  -COPY W0008  -PRE LOPA-                                              
053601     05  FILLER                  PIC X.                                   
053701     EJECT                                                                
053801*01  -COPY W0008  -PRE KVAE-                                              
053901     05  FILLER                  PIC X.                                   
054001     EJECT                                                                
054101*01  -COPY W0008  -PRE KVABSEQ-                                           
054201     05  FILLER                  PIC X.                                   
054301     EJECT                                                                
054401*01  -COPY W0008  -PRE WDK6-                                              
054501     05  FILLER                  PIC X.                                   
054601     EJECT                                                                
054701*01  -COPY W0008  -PRE WDB6-                                              
054801     05  FILLER                  PIC X.                                   
054901     EJECT                                                                
055001 01  PMRK-INLA1-PCB              PIC X.                                   
055101                                                                          
055201 01  PMRK-INLA2-PCB              PIC X.                                   
055301                                                                          
055401 01  PMRK-PLAA-PCB               PIC X.                                   
055501                                                                          
055601 01  KOM-KOMA-PCB                PIC X.                                   
055701                                                                          
055801 01  STYR-HANA-PCB               PIC X.                                   
055901                                                                          
056001 01  STYR-PLAA-PCB               PIC X.                                   
056101                                                                          
056201*01  -COPY W0008  -PRE UPFA-                                              
056301     05  FILLER                  PIC X.                                   
056401     EJECT                                                                
056501 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
056601                           MSG-PCB 6191-PCB 6194-PCB 6195-PCB             
056701                           6197-PCB 6202-PCB DISP-PCB USEA-PCB            
056801                           INLA-PCB INLABSEQ-PCB INLC-PCB                 
056901                           PLAA-PCB LOPA-PCB KVAE-PCB                     
057001                           KVABSEQ-PCB WDK6-PCB WDB6-PCB                  
057101                           PMRK-INLA1-PCB PMRK-INLA2-PCB                  
057201                           PMRK-PLAA-PCB KOM-KOMA-PCB                     
057301                           STYR-HANA-PCB                                  
057401                           STYR-PLAA-PCB UPFA-PCB.                        
057501                                                                          
058001                                                                          
058301     PERFORM A-INIT                                                       
058401     PERFORM B-KOLLA-NYCKLAR                                              
058501     IF NYCKLAR-OK                                                        
058601       IF REQU-UPDATE                                                     
058701         PERFORM G-KOLLA-INPUT                                            
058801         IF INDATA-OK                                                     
058901           PERFORM H-UPPDATERA                                            
059001         END-IF                                                           
059101       ELSE                                                               
059201         IF REQU-FIRST                                                    
059301           CONTINUE                                                       
059401         ELSE                                                             
059501           PERFORM E-SAMMA-SIDA                                           
059601         END-IF                                                           
059701       END-IF                                                             
059801       IF INDATA-OK                                                       
059901         PERFORM F-LAES-VISA-INFO                                         
060001       END-IF                                                             
060101     END-IF                                                               
060501                                                                          
060701     GOBACK                                                               
060801     .                                                                    
060901     EJECT                                                                
061001 A-INIT SECTION.                                                          
061101                                                                          
061201     MOVE ALL '+'       TO RESP-W60136O1                                  
061301     PERFORM MFS-FORM-ATTR                                                
063101     MOVE 001          TO RESP-IDMSGVER                                   
063201     MOVE SPACE        TO RESP-IDMSG-ERROR                                
063301                          RESP-IDMSG-INFO                                 
063401                          RESP-IDELMT-ERROR                               
063501                                                                          
063601     MOVE REQU-KVRADER TO RESP-KVRADER                                    
064701     .                                                                    
064801     EJECT                                                                
065901 B-KOLLA-NYCKLAR SECTION.                                                 
066001                                                                          
066101     MOVE JA TO NYCKLAR-SW                                                
066201                                                                          
066801     IF REQU-IDSPRAK = 'SV'                                               
067001        MOVE +1             TO SPRAK-IX                                   
067201                               WS-KDMFSFOR                                
067301     ELSE                                                                 
067501        MOVE +2             TO SPRAK-IX                                   
067701                               WS-KDMFSFOR                                
067801     END-IF                                                               
067901                                                                          
068001     MOVE REQU-IDLOPNRM-KEY  TO WS-IDLOPNRM                               
068101                                                                          
068201     IF WS-IDLOPNRM NUMERIC AND WS-IDLOPNRM  > ZERO                       
068302       MOVE WS-IDLOPNRM         TO RESP-IDLOPNRM                          
068303       INSPECT RESP-IDLOPNRM REPLACING LEADING ZERO BY SPACE              
068401     ELSE                                                                 
068501       MOVE NEJ TO NYCKLAR-SW                                             
068601     END-IF                                                               
068701                                                                          
068801     MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
068802                           W-IDDC-CN-US                                   
068803                           WS-IDDC                                        
068901     PERFORM IMS-GU-WDB601                                                
069001                                                                          
069101     IF DCS-KDDC = SPACE OR DCS-DDC                                       
069201       MOVE NEJ TO NYCKLAR-SW                                             
069301     ELSE                                                                 
069401       MOVE DCS-IDDC    TO W-IDDC                                         
069501                           W-IDDC-6005                                    
069601     END-IF                                                               
069701     MOVE DCS-IDDC      TO RESP-IDDC                                      
069801                                                                          
070901     IF NYCKLAR-FEL                                                       
071001       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
071301       PERFORM MFS-RENSA-FAELT-IN                                         
071401       PERFORM MFS-RENSA-FAELT-UT                                         
071501     END-IF                                                               
071502                                                                          
071503     IF REQU-ADINLOMR-PRT = ALL '+'                                       
071504       MOVE SPACE             TO RESP-ADINLOMR-PRT                        
071505     ELSE                                                                 
071506       MOVE REQU-ADINLOMR-PRT TO RESP-ADINLOMR-PRT                        
071507     END-IF                                                               
071601     .                                                                    
071701     EJECT                                                                
080001 E-SAMMA-SIDA SECTION.                                                    
080101                                                                          
080301     MOVE NEJ TO INPUT-SW                                                 
080401     IF REQU-FLPREPRA NOT = NEJ                                           
080501       MOVE JA TO INPUT-SW                                                
080601     END-IF                                                               
080701     MOVE +1 TO INDX                                                      
080801     PERFORM UNTIL INDX > REQU-KVRADER                                    
080902       IF REQU-KDFLETI-LINE(INDX) NOT = ALL '+' OR                        
081002          REQU-KVFLETI-LINE(INDX) NOT = ALL '+' OR                        
081102          REQU-KVINLART-LINE1(INDX) NOT = ALL '+'                         
081201         MOVE JA TO INPUT-SW                                              
081301       END-IF                                                             
081401       ADD +1 TO INDX                                                     
081501     END-PERFORM                                                          
081601     IF REQU-KVAVIS-MOT NOT = ALL '+' OR                                  
081701     REQU-IDANSTNR NOT = ALL '+'                                          
081801       MOVE JA TO INPUT-SW                                                
081901     END-IF                                                               
082001                                                                          
082101     IF INGEN-INPUT                                                       
082201       PERFORM MFS-RENSA-FAELT-IN                                         
082301     ELSE                                                                 
082401       MOVE INF-PRESS-PF11 TO RESP-IDMSG-INFO                             
082701       PERFORM EA-MID-INDATA-TILL-MOD                                     
082801     END-IF                                                               
083201     .                                                                    
083301     EJECT                                                                
083401 EA-MID-INDATA-TILL-MOD SECTION.                                          
083501                                                                          
083601* * * * * FÖR VARJE MID-FÄLT                                              
083701* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
083801* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
083901* * * * * ANNARS FLYTTA RENSA-FÄLT TILL RESP-INDATA-FÄLT                  
084001                                                                          
084101     IF REQU-FLPREPRA NOT = ALL '+'                                       
084201       MOVE REQU-FLPREPRA   TO RESP-FLPREPRA                              
084301       MOVE MFS-ADD-LAES-IN-FAELT                                         
084302                            TO RESP-FLPREPRA-ATTR                         
084401     ELSE                                                                 
084501       MOVE ALL-SPACE       TO RESP-FLPREPRA                              
084601     END-IF                                                               
084701                                                                          
084801     IF REQU-FLSVS NOT = ALL '+'                                          
084901       MOVE REQU-FLSVS       TO RESP-FLSVS                                
085001       MOVE MFS-ADD-LAES-IN-FAELT                                         
085101                             TO RESP-FLSVS-ATTR                           
085201     ELSE                                                                 
085301       MOVE ALL-SPACE        TO RESP-FLSVS                                
085401     END-IF                                                               
085501                                                                          
085601     MOVE +1 TO INDX                                                      
085701     PERFORM UNTIL INDX > REQU-KVRADER                                    
085802       IF REQU-KDFLETI-LINE(INDX) NOT = ALL '+'                           
085902         MOVE REQU-KDFLETI-LINE(INDX) TO RESP-KDFLETI-LINE(INDX)          
086002         MOVE MFS-ADD-LAES-IN-FAELT   TO                                  
086102                                   RESP-KDFLETI-LINE-ATTR(INDX)           
086202       ELSE                                                               
086302         MOVE ALL-SPACE       TO RESP-KDFLETI-LINE(INDX)                  
086402       END-IF                                                             
086502       IF REQU-KVFLETI-LINE(INDX) NOT = ALL '+'                           
086602         MOVE REQU-KVFLETI-LINE(INDX) TO RESP-KVFLETI-LINE(INDX)          
086702         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
086802                                   RESP-KVFLETI-LINE-ATTR(INDX)           
086902       ELSE                                                               
087002         MOVE ALL-SPACE       TO RESP-KVFLETI-LINE(INDX)                  
087102       END-IF                                                             
087202       IF REQU-KVINLART-LINE1(INDX) NOT = ALL '+'                         
087302         MOVE REQU-KVINLART-LINE1(INDX)                                   
087303                                    TO RESP-KVINLART-LINE1(INDX)          
087402         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
087502                                   RESP-KVINLART-LINE1-ATTR(INDX)         
087602       ELSE                                                               
087702         MOVE ALL-SPACE             TO RESP-KVINLART-LINE1(INDX)          
087802       END-IF                                                             
087902       IF REQU-KDKLIPRI-LINE(INDX) NOT = ALL '+'                          
088002         MOVE REQU-KDKLIPRI-LINE(INDX) TO RESP-KDKLIPRI-LINE(INDX)        
088102         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
088202                                 RESP-KDKLIPRI-LINE-ATTR(INDX)            
088302       ELSE                                                               
088402         MOVE ALL-SPACE       TO RESP-KDKLIPRI-LINE(INDX)                 
088502       END-IF                                                             
088602       IF REQU-FLSATS-LINE(INDX) NOT = ALL '+'                            
088702         MOVE REQU-FLSATS-LINE(INDX) TO RESP-FLSATS-LINE(INDX)            
088802         MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLSATS-LINE-ATTR(INDX)        
088902       ELSE                                                               
089002         MOVE ALL-SPACE       TO RESP-FLSATS-LINE(INDX)                   
089102       END-IF                                                             
089202       IF REQU-FLPREPKL-LINE(INDX) NOT = ALL '+'                          
089302         MOVE REQU-FLPREPKL-LINE(INDX) TO RESP-FLPREPKL-LINE(INDX)        
089402         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
089502                                  RESP-FLPREPKL-LINE-ATTR(INDX)           
089602       ELSE                                                               
089702         MOVE ALL-SPACE       TO  RESP-FLPREPKL-LINE(INDX)                
089802       END-IF                                                             
089902       ADD +1 TO INDX                                                     
090002     END-PERFORM                                                          
090102                                                                          
090202     IF REQU-KVAVIS-MOT NOT = ALL '+'                                     
090302       MOVE REQU-KVAVIS-MOT TO RESP-KVAVIS-MOT                            
090402       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KVAVIS-MOT-ATTR                 
090502     ELSE                                                                 
090602       MOVE ALL-SPACE       TO RESP-KVAVIS-MOT                            
090702     END-IF                                                               
090802                                                                          
090902     IF REQU-IDANSTNR NOT = ALL '+'                                       
091002         MOVE REQU-IDANSTNR TO RESP-IDANSTNR                              
091102         MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDANSTNR-ATTR                 
091202       ELSE                                                               
091302         MOVE ALL-SPACE       TO RESP-IDANSTNR                            
091402     END-IF                                                               
091502                                                                          
091602     .                                                                    
091702     EJECT                                                                
091802 F-LAES-VISA-INFO SECTION.                                                
091902                                                                          
092002     PERFORM FA-LAES-GRUNDDATA                                            
092102                                                                          
092202     IF SEGMENT-SAKNAS OR PARTI-FEL                                       
092301       PERFORM MFS-RENSA-FAELT-UT                                         
092401     ELSE                                                                 
092501       PERFORM FB-LAEGG-UT-GRUNDDATA                                      
092601       IF REQU-UPDATE OR REQU-FIRST                                       
092701         PERFORM MFS-RENSA-FAELT-IN                                       
092801       END-IF                                                             
092901     END-IF                                                               
092902     IF RESP-IDMSG-ERROR = ERR-MISSING                                    
092903        MOVE 0 TO RESP-KVRADER                                            
092904     END-IF                                                               
093001     .                                                                    
093101     EJECT                                                                
093201 FA-LAES-GRUNDDATA SECTION.                                               
093301                                                                          
093401     MOVE WS-IDLOPNRM TO W-IDLOPNRM                                       
093501     PERFORM IMS-GU-INLA-W6D111                                           
093601     IF SEGMENT-FINNS                                                     
093701       MOVE ZERO          TO STYR-IDARTNR                                 
093801                             STYR-IDFKNGRP                                
093901       MOVE SPACE         TO STYR-IDLEVNR                                 
094001       MOVE INLA-ART-IDDC TO STYR-IDDC                                    
094101       MOVE INLA-ART-BEFT TO STYR-BEFT                                    
094201       CALL W611STYR USING STYR-W611STYR STYR-HANA-PCB                    
094301                                         STYR-PLAA-PCB                    
094401       IF STYR-ADINLOMR-FP = SPACE                                        
094501         MOVE NEJ TO FPK-SW                                               
094601       END-IF                                                             
094701       MOVE DLI-IO-AREA TO DLI-WS-AREA                                    
094801       IF INLA-ART-FLKLAR = JA                                            
094901         MOVE JA TO PARTI-SW                                              
095001         MOVE ERR-MISSING TO RESP-IDMSG-ERROR                             
095101       ELSE                                                               
095201         PERFORM IMS-GNP-OKVAL-INLA-W6D121                                
095301         IF INLA-RAD-IDRADNR = 1 AND INLA-RAD-IDOKOLLI > ZERO             
095401           MOVE JA TO PARTI-SW                                            
095501           MOVE ERR-DIV-CASE TO RESP-IDMSG-ERROR                          
095601         END-IF                                                           
095701       END-IF                                                             
095801     ELSE                                                                 
095901       MOVE ERR-MISSING TO RESP-IDMSG-ERROR                               
096001     END-IF                                                               
096101                                                                          
096201     .                                                                    
096301     EJECT                                                                
096401 FB-LAEGG-UT-GRUNDDATA SECTION.                                           
096501                                                                          
096601     MOVE WS-INLA-ART-IDARTNR  TO RESP-IDARTNR                            
096701                                  W-IDARTNR                               
096801     MOVE WS-INLA-ART-KVAVIS   TO RESP-KVAVIS                             
096901     MOVE WS-INLA-ART-BEART    TO RESP-BEART                              
097001     MOVE WS-INLA-ART-KDSORT   TO RESP-KDSORT                             
097101     MOVE WS-INLA-ART-BEFT     TO RESP-BEFT                               
097201     MOVE SPACE                TO RESP-BEFARLIG                           
097301     IF WS-INLA-ART-KDFARLIG = 4                                          
097401     OR WS-INLA-ART-KDFARLIG = 7                                          
097501       IF WS-ENGLISH-TEXT                                                 
097601          MOVE YES             TO RESP-BEFARLIG                           
097701       ELSE                                                               
097801          MOVE JA              TO RESP-BEFARLIG                           
097901       END-IF                                                             
098001     END-IF                                                               
098101     IF WS-INLA-ART-KDFARLIG = 5                                          
098201       MOVE 'ASBEST'           TO RESP-BEFARLIG                           
098301     END-IF                                                               
098401     IF WS-INLA-ART-KDFARLIG = 6                                          
098501       IF WS-ENGLISH-TEXT                                                 
098601         MOVE 'KEMIKALIER'     TO RESP-BEFARLIG                           
098701       ELSE                                                               
098801         MOVE 'CHEMICALS '     TO RESP-BEFARLIG                           
098901       END-IF                                                             
099001     END-IF                                                               
099101     MOVE WS-INLA-ART-KDLAGEMB     TO RESP-KDLAGEMB                       
099201     MOVE WS-INLA-ART-ADLAGOMR     TO RESP-ADLAGOMR                       
099301     MOVE WS-INLA-ART-ADGANG       TO RESP-ADGANG                         
099401     MOVE WS-INLA-ART-ADPLATS      TO RESP-ADPLATS                        
099501     MOVE WS-INLA-ART-ADTRDEST-KIT TO RESP-ADTRDEST-KIT                   
099601     IF WS-INLA-ART-KDKVAANT > ZERO                                       
099701       IF WS-ENGLISH-TEXT                                                 
099801          MOVE YES             TO RESP-FLKVAANT-TOT                       
099901       ELSE                                                               
100001          MOVE JA              TO RESP-FLKVAANT-TOT                       
100101       END-IF                                                             
100201     ELSE                                                                 
100301       MOVE NEJ             TO RESP-FLKVAANT-TOT                          
100401     END-IF                                                               
100501                                                                          
100601     PERFORM FBA-SUMMERA-RADER                                            
100701                                                                          
100801     MOVE ZERO           TO RESP-KVAVIS-KVAR                              
100901     COMPUTE WS-KVAVIS-VERKL =                                            
101001     WS-INLA-ART-KVAVIS - WS-KVAVIS-RAKN                                  
101101     IF WS-KVAVIS-VERKL > ZERO                                            
101201       MOVE WS-KVAVIS-VERKL TO RESP-KVAVIS-KVAR                           
101301     END-IF                                                               
101401                                                                          
101501     MOVE ZERO               TO RESP-KVAVIS-FPK-KVAR                      
101601     IF FPK-PARTI                                                         
101701       IF WS-INLA-ART-KVAVIS-KIT > WS-KIT-RAKN                            
101801*****DVS OM DET FINNS SATS KVAR ATT MÄRKA,                                
101901*****SÅ UNDANTAG DETTA FRÅN DET SOM SKA FÖRPACKAS                         
102001         COMPUTE WS-KVAVIS-FPK-KVAR =                                     
102101         WS-FPK-RAKN - (WS-INLA-ART-KVAVIS-KIT - WS-KIT-RAKN)             
102201       ELSE                                                               
102301         MOVE WS-FPK-RAKN TO WS-KVAVIS-FPK-KVAR                           
102401       END-IF                                                             
102501       IF WS-KVAVIS-FPK-KVAR > ZERO                                       
102601         MOVE WS-KVAVIS-FPK-KVAR TO RESP-KVAVIS-FPK-KVAR                  
102701       END-IF                                                             
102801     END-IF                                                               
102901                                                                          
103001     COMPUTE WS-KVAVIS-PRIO-KVAR =                                        
103101     WS-INLA-ART-KVAVIS-PRIO - WS-PRIO-RAKN                               
103201     IF WS-KVAVIS-PRIO-KVAR > ZERO                                        
103301       MOVE WS-KVAVIS-PRIO-KVAR TO RESP-KVAVIS-PRIO-KVAR                  
103401     ELSE                                                                 
103501       MOVE ZERO                TO RESP-KVAVIS-PRIO-KVAR                  
103601     END-IF                                                               
103701                                                                          
103801     COMPUTE WS-KVAVIS-KIT-KVAR =                                         
103901     WS-INLA-ART-KVAVIS-KIT - WS-KIT-RAKN                                 
104001     IF WS-KVAVIS-KIT-KVAR > ZERO                                         
104101       MOVE WS-KVAVIS-KIT-KVAR TO RESP-KVAVIS-KIT-KVAR                    
104201     ELSE                                                                 
104301       MOVE ZERO               TO RESP-KVAVIS-KIT-KVAR                    
104401     END-IF                                                               
104501                                                                          
104601     IF INGEN-INPUT OR REQU-UPDATE                                        
104701       MOVE NEJ TO RESP-FLPREPRA                                          
104801       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLPREPRA-ATTR                   
104901                                                                          
105001       MOVE +1 TO INDX                                                    
105101       PERFORM UNTIL INDX > REQU-KVRADER                                  
105202         MOVE NEJ TO RESP-KDKLIPRI-LINE(INDX)                             
105302                     RESP-FLSATS-LINE(INDX)                               
105401         IF FPK-PARTI                                                     
105501           IF WS-ENGLISH-TEXT                                             
105602              MOVE YES TO RESP-FLPREPKL-LINE(INDX)                        
105701           ELSE                                                           
105802              MOVE JA TO RESP-FLPREPKL-LINE(INDX)                         
105901           END-IF                                                         
106001         ELSE                                                             
106102           MOVE NEJ TO RESP-FLPREPKL-LINE(INDX)                           
106201         END-IF                                                           
106302         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
106402                                 RESP-KDKLIPRI-LINE-ATTR(INDX)            
106502                                 RESP-FLSATS-LINE-ATTR(INDX)              
106602                                 RESP-FLPREPKL-LINE-ATTR(INDX)            
106702         ADD +1 TO INDX                                                   
106802       END-PERFORM                                                        
106902     END-IF                                                               
107002                                                                          
107102     IF DCS-CDC                                                           
107202       PERFORM IMS-GU-WDK611                                              
107302       IF SEGMENT-FINNS                                                   
107402         MOVE CLAG-KVROS TO RESP-KVROS                                    
107502       END-IF                                                             
107602     END-IF                                                               
107702     .                                                                    
107802     EJECT                                                                
107902 FBA-SUMMERA-RADER SECTION.                                               
108002                                                                          
108102***** 1:A LÄSNINGEN GÖRS I FA-LAES                                        
108202     PERFORM UNTIL SEGMENT-SAKNAS                                         
108302       IF INLA-RAD-KDINLSTA = 'ANT' OR 'AVV' OR 'KVA'                     
108402         ADD INLA-RAD-KVINLART TO WS-KVAVIS-RAKN                          
108502       END-IF                                                             
108602       IF (INLA-RAD-KDINLSTA = SPACE OR 'SAK') AND                        
108702          INLA-RAD-FLSATS = NEJ                                           
108802         ADD INLA-RAD-KVINLART TO WS-FPK-RAKN                             
108902       END-IF                                                             
109002       IF FPK-PARTI                                                       
109102         IF INLA-RAD-FLPRIO = JA AND                                      
109202           (INLA-RAD-KDINLSTA = 'FPK' OR 'INL')                           
109302           ADD INLA-RAD-KVINLART TO WS-PRIO-RAKN                          
109402         END-IF                                                           
109502       ELSE                                                               
109602         IF INLA-RAD-FLPRIO = JA AND                                      
109702           (INLA-RAD-KDINLSTA = 'FPK' OR SPACE OR 'INL')                  
109802           ADD INLA-RAD-KVINLART TO WS-PRIO-RAKN                          
109902         END-IF                                                           
110002       END-IF                                                             
110102       IF INLA-RAD-FLSATS = JA                                            
110202         ADD INLA-RAD-KVINLART TO WS-KIT-RAKN                             
110302       END-IF                                                             
110402       PERFORM IMS-GNP-OKVAL-INLA-W6D121                                  
110502     END-PERFORM                                                          
110602                                                                          
110702     .                                                                    
110802     EJECT                                                                
110902 G-KOLLA-INPUT SECTION.                                                   
111002                                                                          
111102     MOVE JA  TO INDATA-SW                                                
111202     MOVE NEJ TO INPUT-SW                                                 
111302     IF REQU-FLPREPRA NOT = NEJ                                           
111402       MOVE JA TO INPUT-SW                                                
111502     END-IF                                                               
111602     MOVE +1 TO INDX                                                      
111702     PERFORM UNTIL INDX > REQU-KVRADER                                    
111802       IF REQU-KDFLETI-LINE(INDX) = ALL '+' AND                           
111902          REQU-KVFLETI-LINE(INDX) = ALL '+' AND                           
112002          REQU-KVINLART-LINE1(INDX) = ALL '+'                             
112102         CONTINUE                                                         
112202       ELSE                                                               
112302         MOVE JA TO INPUT-SW                                              
112402                    RADER-SW                                              
112502       END-IF                                                             
112602       ADD +1 TO INDX                                                     
112702     END-PERFORM                                                          
112802     IF REQU-KVAVIS-MOT = ALL '+' AND                                     
112902     REQU-IDANSTNR = ALL '+'                                              
113002         CONTINUE                                                         
113102     ELSE                                                                 
113202       MOVE JA TO INPUT-SW                                                
113302                  AVVIK-SW                                                
113402     END-IF                                                               
113502                                                                          
113602     IF INGEN-INPUT                                                       
113702       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
113802       PERFORM MFS-ROER-EJ-FAELT-IN                                       
113902       PERFORM MFS-ROER-EJ-FAELT-UT                                       
114002       PERFORM MFS-LAES-IN-IGEN                                           
114102       MOVE NEJ TO INDATA-SW                                              
114202     ELSE                                                                 
114302       IF REQU-FLPREPRA NOT = ALL '+'                                     
114402         IF REQU-FLPREPRA = NEJ OR JA OR YES                              
114502           MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLPREPRA-ATTR                
114602         ELSE                                                             
114702           MOVE MFS-ALFA-FAELT-FEL TO RESP-FLPREPRA-ATTR                  
114802           MOVE NEJ TO INDATA-SW                                          
114902         END-IF                                                           
115002       END-IF                                                             
115102       IF REQU-FLSVS NOT = ALL '+'                                        
115202         IF REQU-FLSVS = NEJ OR JA OR YES OR SPACE                        
115302           MOVE MFS-ALFA-FAELT-RAETT                                      
115402                             TO RESP-FLSVS-ATTR                           
115502         ELSE                                                             
115602           MOVE MFS-ALFA-FAELT-FEL                                        
115702                             TO RESP-FLSVS-ATTR                           
115802           MOVE NEJ TO INDATA-SW                                          
115902         END-IF                                                           
116002       END-IF                                                             
116102       IF INDATA-FEL                                                      
116202         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
116301         PERFORM MFS-ROER-EJ-FAELT-UT                                     
116401         PERFORM MFS-ROER-EJ-FAELT-IN                                     
116501       ELSE                                                               
116601         MOVE +1 TO INDX                                                  
116701         PERFORM UNTIL INDX > REQU-KVRADER                                
116801           PERFORM GA-KOLLA-RADER                                         
116901           ADD +1 TO INDX                                                 
117001         END-PERFORM                                                      
117101         IF INDATA-FEL                                                    
117201           MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                  
117501           PERFORM MFS-ROER-EJ-FAELT-UT                                   
117601           PERFORM MFS-ROER-EJ-FAELT-IN                                   
117701         ELSE                                                             
117801           PERFORM GB-KOLLA-AVVIK                                         
117901           IF INDATA-FEL                                                  
118001             MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                
118301             PERFORM MFS-ROER-EJ-FAELT-UT                                 
118401             PERFORM MFS-ROER-EJ-FAELT-IN                                 
118501           ELSE                                                           
118601             PERFORM GC-KOLLA-REGLER                                      
118701             IF INDATA-FEL                                                
118801***FELMEDDELADE-FLYTT GÖRS I GC-KOLLA-...                                 
119101               PERFORM MFS-ROER-EJ-FAELT-IN                               
119201               PERFORM MFS-ROER-EJ-FAELT-UT                               
119301             END-IF                                                       
119401           END-IF                                                         
119501         END-IF                                                           
119601       END-IF                                                             
119701     END-IF                                                               
119801                                                                          
119901     .                                                                    
120001     EJECT                                                                
120101 GA-KOLLA-RADER SECTION.                                                  
120201                                                                          
120302     IF REQU-KDFLETI-LINE(INDX) NOT = ALL '+'                             
120402       IF REQU-KDFLETI-LINE(INDX) = FLAGGA OR ETIKETT OR                  
120501                               LAB   OR MLABEL                            
120602         IF REQU-KDFLETI-LINE(INDX) = FLAGGA OR LAB                       
120701           MOVE JA TO FLAGG-SW                                            
120801         END-IF                                                           
120902         IF REQU-KDFLETI-LINE(INDX) = ETIKETT OR MLABEL                   
121001           MOVE JA TO ETIKETT-SW                                          
121101         END-IF                                                           
121201         MOVE MFS-ALFA-FAELT-RAETT TO                                     
121302         RESP-KDFLETI-LINE-ATTR(INDX)                                     
121402         IF REQU-KVFLETI-LINE(INDX) = ALL '+' OR                          
121502            REQU-KVINLART-LINE1(INDX) = ALL '+'                           
121602           IF REQU-KVFLETI-LINE(INDX) = ALL '+'                           
121702             MOVE MFS-NUM-FAELT-FEL TO                                    
121802                                     RESP-KVFLETI-LINE-ATTR(INDX)         
121901             MOVE NEJ TO INDATA-SW                                        
122001           END-IF                                                         
122104           IF REQU-KVINLART-LINE1(INDX) = ALL '+'                         
122202             MOVE MFS-NUM-FAELT-FEL TO                                    
122302                                   RESP-KVINLART-LINE1-ATTR(INDX)         
122401             MOVE NEJ TO INDATA-SW                                        
122501           END-IF                                                         
122601         END-IF                                                           
122701       ELSE                                                               
122802         MOVE MFS-ALFA-FAELT-FEL TO RESP-KDFLETI-LINE-ATTR(INDX)          
122901         MOVE NEJ TO INDATA-SW                                            
123001       END-IF                                                             
123101     END-IF                                                               
123202     IF REQU-KVFLETI-LINE(INDX) NOT = ALL '+'                             
123302       IF REQU-KVFLETI-LINE(INDX) NUMERIC AND                             
123402          REQU-KVFLETI-LINE(INDX) > ZERO                                  
123502         MOVE MFS-NUM-FAELT-RAETT TO RESP-KVFLETI-LINE-ATTR(INDX)         
123602         MOVE REQU-KVFLETI-LINE(INDX) TO WS-KVFLETI(INDX)                 
123702         IF REQU-KDFLETI-LINE(INDX) = ALL '+' OR                          
123802            REQU-KVINLART-LINE1(INDX) = ALL '+'                           
123902           IF REQU-KDFLETI-LINE(INDX) = ALL '+'                           
124002             MOVE MFS-ALFA-FAELT-FEL TO                                   
124102                                     RESP-KDFLETI-LINE-ATTR(INDX)         
124201             MOVE NEJ TO INDATA-SW                                        
124301           END-IF                                                         
124402           IF REQU-KVINLART-LINE1(INDX) = ALL '+'                         
124502             MOVE MFS-NUM-FAELT-FEL TO                                    
124602                                    RESP-KVINLART-LINE1-ATTR(INDX)        
124701             MOVE NEJ TO INDATA-SW                                        
124801           END-IF                                                         
124901         END-IF                                                           
125001       ELSE                                                               
125102         MOVE MFS-NUM-FAELT-FEL TO RESP-KVFLETI-LINE-ATTR(INDX)           
125201         MOVE NEJ TO INDATA-SW                                            
125301       END-IF                                                             
125401     END-IF                                                               
125502     IF REQU-KVINLART-LINE1(INDX) NOT = ALL '+'                           
125602       IF REQU-KVINLART-LINE1(INDX) NUMERIC AND                           
125702          REQU-KVINLART-LINE1(INDX) > ZERO                                
125801         MOVE MFS-NUM-FAELT-RAETT TO                                      
125902                                   RESP-KVINLART-LINE1-ATTR(INDX)         
126002         MOVE REQU-KVINLART-LINE1(INDX) TO WS-KVINLART(INDX)              
126102         IF REQU-KDFLETI-LINE(INDX) = ALL '+' OR                          
126202            REQU-KVFLETI-LINE(INDX) = ALL '+'                             
126302           IF REQU-KDFLETI-LINE(INDX) = ALL '+'                           
126402             MOVE MFS-ALFA-FAELT-FEL TO                                   
126502                                     RESP-KDFLETI-LINE-ATTR(INDX)         
126601             MOVE NEJ TO INDATA-SW                                        
126701           END-IF                                                         
126802           IF REQU-KVFLETI-LINE(INDX) = ALL '+'                           
126902             MOVE MFS-NUM-FAELT-FEL TO                                    
127002                                     RESP-KVFLETI-LINE-ATTR(INDX)         
127101             MOVE NEJ TO INDATA-SW                                        
127201           END-IF                                                         
127301         END-IF                                                           
127401       ELSE                                                               
127502         MOVE MFS-NUM-FAELT-FEL TO RESP-KVINLART-LINE1-ATTR(INDX)         
127601         MOVE NEJ TO INDATA-SW                                            
127701       END-IF                                                             
127801     END-IF                                                               
127902     IF REQU-KDKLIPRI-LINE(INDX) NOT = ALL '+'                            
128002       IF REQU-KDKLIPRI-LINE(INDX) = YES OR JA OR NEJ OR PRIO             
128101         MOVE MFS-ALFA-FAELT-RAETT TO                                     
128202         RESP-KDKLIPRI-LINE-ATTR(INDX)                                    
128301       ELSE                                                               
128402         MOVE MFS-ALFA-FAELT-FEL TO RESP-KDKLIPRI-LINE-ATTR(INDX)         
128501         MOVE NEJ TO INDATA-SW                                            
128601       END-IF                                                             
128701     END-IF                                                               
128802     IF REQU-FLSATS-LINE(INDX) NOT = ALL '+'                              
128902       IF REQU-FLSATS-LINE(INDX) = YES OR JA OR NEJ                       
129002         MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLSATS-LINE-ATTR(INDX)         
129101       ELSE                                                               
129202         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLSATS-LINE-ATTR(INDX)           
129301         MOVE NEJ TO INDATA-SW                                            
129401       END-IF                                                             
129501     END-IF                                                               
129602     IF REQU-FLPREPKL-LINE(INDX) NOT = ALL '+'                            
129702       IF REQU-FLPREPKL-LINE(INDX) = YES OR JA OR NEJ                     
129801         MOVE MFS-ALFA-FAELT-RAETT TO                                     
129902         RESP-FLPREPKL-LINE-ATTR(INDX)                                    
130001       ELSE                                                               
130102         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLPREPKL-LINE-ATTR(INDX)         
130201         MOVE NEJ TO INDATA-SW                                            
130301       END-IF                                                             
130401     END-IF                                                               
130501                                                                          
130601     .                                                                    
130701     EJECT                                                                
130801 GB-KOLLA-AVVIK SECTION.                                                  
130901                                                                          
131001     IF REQU-KVAVIS-MOT NOT = ALL '+'                                     
131101       IF REQU-KVAVIS-MOT NUMERIC AND REQU-KVAVIS-MOT > ZERO AND          
131201          REQU-IDANSTNR NOT = ALL '+'                                     
131301         MOVE MFS-NUM-FAELT-RAETT TO RESP-KVAVIS-MOT-ATTR                 
131401         MOVE REQU-KVAVIS-MOT TO WS-KVAVIS-MOT                            
131501       ELSE                                                               
131601         MOVE MFS-NUM-FAELT-FEL TO RESP-KVAVIS-MOT-ATTR                   
131701                                   RESP-IDANSTNR-ATTR                     
131801         MOVE NEJ TO INDATA-SW                                            
131901       END-IF                                                             
132001     END-IF                                                               
132101                                                                          
132201     IF REQU-IDANSTNR NOT = ALL '+'                                       
132301       IF REQU-IDANSTNR NUMERIC AND REQU-IDANSTNR > ZERO AND              
132401          REQU-KVAVIS-MOT NOT = ALL '+'                                   
132501         MOVE MFS-NUM-FAELT-RAETT TO RESP-IDANSTNR-ATTR                   
132601       ELSE                                                               
132701         MOVE MFS-NUM-FAELT-FEL TO RESP-IDANSTNR-ATTR                     
132801                                   RESP-KVAVIS-MOT-ATTR                   
132901         MOVE NEJ TO INDATA-SW                                            
133001       END-IF                                                             
133101     END-IF                                                               
133201                                                                          
133301     .                                                                    
133401     EJECT                                                                
133501 GC-KOLLA-REGLER SECTION.                                                 
133601                                                                          
133701     IF REQU-FLPREPRA = YES OR JA OR RADER-IFYLLDA                        
133702       IF REQU-IDMSGVER = 1                                               
133703         CONTINUE                                                         
133704       ELSE                                                               
133801         PERFORM GCA-KOLLA-PRINTER                                        
133901       END-IF                                                             
133902     END-IF                                                               
134001                                                                          
134101     MOVE +1 TO INDX                                                      
134201     PERFORM UNTIL INDX > REQU-KVRADER                                    
134302       IF REQU-FLPREPKL-LINE(INDX) = NEJ                                  
134402       AND REQU-FLSATS-LINE(INDX) = NEJ                                   
134501         MOVE JA TO OMPACKNINGS-SW                                        
134601       END-IF                                                             
134701       ADD +1 TO INDX                                                     
134801     END-PERFORM                                                          
134901                                                                          
135001     MOVE WS-IDLOPNRM TO W-IDLOPNRM                                       
135101     PERFORM IMS-GU-INLA-W6D111                                           
135201     IF SEGMENT-FINNS                                                     
135301       PERFORM IMS-GNP-OKVAL-INLA-W6D121                                  
135401       PERFORM UNTIL SEGMENT-SAKNAS                                       
135501         IF INLA-RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE OR 'VOR'          
135601                                OR 'INL' OR 'KVA' OR 'RET'                
135701                                OR 'FRD'                                  
135801           ADD INLA-RAD-KVINLART TO WS-KVAVIS-VERKL                       
135901           IF (INLA-RAD-KDINLSTA = 'SAK' OR SPACE) AND                    
136001              (NOT OMPACKNING) AND (INLA-RAD-IDRADNR NOT = +1)            
136101             MOVE JA TO KOLLI-KVAR-SW                                     
136201           END-IF                                                         
136301         END-IF                                                           
136401         PERFORM IMS-GNP-OKVAL-INLA-W6D121                                
136501       END-PERFORM                                                        
136601     ELSE                                                                 
136701       MOVE NEJ TO INDATA-SW                                              
136801       MOVE ERR-MISSING TO RESP-IDMSG-ERROR                               
136901     END-IF                                                               
137001                                                                          
137101     IF INDATA-FEL                                                        
137201       PERFORM MFS-ROER-EJ-FAELT-IN                                       
137301       PERFORM MFS-ROER-EJ-FAELT-UT                                       
137401     ELSE                                                                 
137501       MOVE WS-IDLOPNRM TO W-IDLOPNRM                                     
137601       PERFORM IMS-GU-INLA-W6D111                                         
137701       MOVE DLI-IO-AREA TO DLI-WS-AREA                                    
137801       MOVE +1          TO W-IDRADNR                                      
137901       PERFORM IMS-GNP-KVAL-INLA-W6D121                                   
138001       IF SEGMENT-SAKNAS                                                  
138101         MOVE ZERO TO WS-RAD1-KVINLART                                    
138201         PERFORM IMS-GNP-OKVAL-INLA-W6D121                                
138301       ELSE                                                               
138401         MOVE INLA-RAD-KVINLART TO WS-RAD1-KVINLART                       
138501       END-IF                                                             
138601       PERFORM S50-PRIM-CONTROL                                           
138701       IF INDATA-OK                                                       
138801         IF RADER-IFYLLDA                                                 
138901           PERFORM GCB-KOLLA-REGLER-RADER                                 
139001         END-IF                                                           
139101                                                                          
139201         IF (REQU-FLSVS = YES                                             
139301         OR REQU-FLSVS = JA)                                              
139401           IF RADER-IFYLLDA OR REQU-FLPREPRA = JA                         
139501             MOVE WS-INLA-ART-IDARTNR                                     
139601                               TO W-IDARTNR                               
139701             PERFORM IMS-GU-WDK611                                        
139801             IF SEGMENT-FINNS                                             
139901             AND CLAG-ADLAGOMR-SVS > ZERO                                 
140001               CONTINUE                                                   
140101             ELSE                                                         
140201               MOVE MFS-ALFA-FAELT-FEL                                    
140301                               TO RESP-FLSVS-ATTR                         
140401               MOVE NEJ      TO INDATA-SW                                 
140501             END-IF                                                       
140601           ELSE                                                           
140701             MOVE MFS-ALFA-FAELT-FEL                                      
140801                               TO RESP-FLSVS-ATTR                         
140901             MOVE NEJ        TO INDATA-SW                                 
141001           END-IF                                                         
141101         END-IF                                                           
141201       END-IF                                                             
141301                                                                          
141401       IF INDATA-FEL                                                      
141501         PERFORM MFS-ROER-EJ-FAELT-IN                                     
141601         PERFORM MFS-ROER-EJ-FAELT-UT                                     
141701       ELSE                                                               
141801         IF AVVIK-IFYLLD                                                  
141901           PERFORM GCC-KOLLA-REGLER-AVVIK                                 
142001         END-IF                                                           
142101       END-IF                                                             
142201     END-IF                                                               
142301                                                                          
142401     .                                                                    
142501     EJECT                                                                
142601 GCA-KOLLA-PRINTER SECTION.                                               
142701                                                                          
142801     IF REQU-ADINLOMR-PRT NOT = ALL '+'                                   
142901       MOVE REQU-ADINLOMR-PRT TO W-ADINLOMR                               
143001                                W-ADINLOMR-PRT                            
143101       PERFORM IMS-GU-PLAA-W6G130                                         
143201       IF SEGMENT-SAKNAS                                                  
143301         MOVE MFS-ALFA-FAELT-FEL TO RESP-ADINLOMR-PRT-ATTR                
143401         MOVE NEJ TO INDATA-SW                                            
143501         MOVE ERR-PRT-MISSING TO RESP-IDMSG-ERROR                         
143601       ELSE                                                               
143701         MOVE MFS-ALFA-FAELT-RAETT TO RESP-ADINLOMR-PRT-ATTR              
143801         MOVE PLAA-6006-IDLEVNR    TO SPAR-IDLEVNR                        
143901       END-IF                                                             
144001     ELSE                                                                 
144101       MOVE MFS-ALFA-FAELT-FEL TO RESP-ADINLOMR-PRT-ATTR                  
144201       MOVE NEJ                TO INDATA-SW                               
144301       MOVE ERR-PRT-MISSING    TO RESP-IDMSG-ERROR                        
144401     END-IF                                                               
144501                                                                          
144601     IF INDATA-OK                                                         
144701       IF REQU-FLPREPRA = JA OR YES                                       
144801         MOVE 001            TO PRT-KDCALL                                
144901         MOVE SPACE          TO PRT-IDPRTLST                              
145001         MOVE SPACE          TO SPAR-IDPRTLST-VAL                         
145101         MOVE '6L'           TO PRT-IDPRTLST(1:2)                         
145201         MOVE W-ADINLOMR-PRT TO PRT-IDPRTLST(3:6)                         
145301         CALL W006PRT USING PRT-W006PRT                                   
145401         IF PRT-KDSVAR = 'F'                                              
145501            MOVE 001            TO PRT-KDCALL                             
145601            MOVE SPACE          TO PRT-IDPRTLST                           
145701            MOVE '6F'           TO PRT-IDPRTLST(1:2)                      
145801            MOVE W-ADINLOMR-PRT TO PRT-IDPRTLST(3:6)                      
145901            CALL W006PRT USING PRT-W006PRT                                
146001            IF PRT-KDSVAR = 'F' OR DCS-CDC-TR                             
146101              MOVE NEJ                TO INDATA-SW                        
146201              MOVE MFS-ALFA-FAELT-FEL TO RESP-ADINLOMR-PRT-ATTR           
146301              MOVE ERR-PRT-MISSING    TO RESP-IDMSG-ERROR                 
146401            ELSE                                                          
146501              MOVE MFS-ALFA-FAELT-RAETT  TO RESP-ADINLOMR-PRT-ATTR        
146601              MOVE PRT-BEPRTLST          TO RESP-BEPRTLST                 
146701              MOVE '6F'                  TO SPAR-IDPRTLST-VAL             
146801            END-IF                                                        
146901         ELSE                                                             
147001           MOVE MFS-ALFA-FAELT-RAETT  TO RESP-ADINLOMR-PRT-ATTR           
147101           MOVE PRT-BEPRTLST          TO RESP-BEPRTLST                    
147201           MOVE '6L'                  TO SPAR-IDPRTLST-VAL                
147301         END-IF                                                           
147401       END-IF                                                             
147501     END-IF                                                               
147601                                                                          
147701     IF INDATA-OK                                                         
147801       IF FLAGGA-VALD                                                     
147901         MOVE 001        TO PRT-KDCALL                                    
148001         MOVE '6F'       TO PRT-IDPRTLST(1:2)                             
148101         MOVE W-ADINLOMR-PRT TO PRT-IDPRTLST(3:6)                         
148201         CALL W006PRT USING PRT-W006PRT                                   
148301         IF PRT-KDSVAR = 'F'                                              
148401           MOVE MFS-ALFA-FAELT-FEL TO RESP-ADINLOMR-PRT-ATTR              
148501           MOVE NEJ                TO INDATA-SW                           
148601           MOVE ERR-PRT-MISSING    TO RESP-IDMSG-ERROR                    
148701         ELSE                                                             
148801           MOVE PRT-BEPRTLST       TO RESP-BEPRTLST                       
148901         END-IF                                                           
149001       END-IF                                                             
149101     END-IF                                                               
149201                                                                          
149301     IF INDATA-OK                                                         
149401       IF ETIKETT-VALD                                                    
149501         MOVE 001            TO PRT-KDCALL                                
149601         MOVE '6E'           TO PRT-IDPRTLST(1:2)                         
149701         MOVE W-ADINLOMR-PRT TO PRT-IDPRTLST(3:6)                         
149801         CALL W006PRT USING PRT-W006PRT                                   
149901         IF PRT-KDSVAR = 'F'                                              
150001           MOVE MFS-ALFA-FAELT-FEL TO RESP-ADINLOMR-PRT-ATTR              
150101           MOVE NEJ                TO INDATA-SW                           
150201           MOVE ERR-PRT-MISSING    TO RESP-IDMSG-ERROR                    
150301         ELSE                                                             
150401           MOVE PRT-BEPRTLST       TO RESP-BEPRTLST                       
150501         END-IF                                                           
150601       END-IF                                                             
150701     END-IF                                                               
150801                                                                          
150901     .                                                                    
151001     EJECT                                                                
151101 GCB-KOLLA-REGLER-RADER SECTION.                                          
151201                                                                          
151301     MOVE +1 TO INDX                                                      
151401     PERFORM UNTIL INDX > REQU-KVRADER                                    
151501       COMPUTE WS-SUMMA-ART-BILD =                                        
151601       WS-SUMMA-ART-BILD +                                                
151701       (WS-KVFLETI(INDX) * WS-KVINLART(INDX))                             
151801       COMPUTE WS-KVFLETI-TOT = WS-KVFLETI-TOT + WS-KVFLETI(INDX)         
151901       ADD +1 TO INDX                                                     
152001     END-PERFORM                                                          
152101                                                                          
152201     IF WS-KVFLETI-TOT > 100                                              
152301       MOVE NEJ TO INDATA-SW                                              
152401       MOVE ERR-TOO-MANY TO RESP-IDMSG-ERROR                              
152501     END-IF                                                               
152601                                                                          
152701     MOVE ZERO TO WS-DIFFERENS                                            
152801     IF AVVIK-IFYLLD                                                      
152901       COMPUTE WS-DIFFERENS =                                             
153001       WS-KVAVIS-VERKL + (WS-SUMMA-ART-BILD - WS-RAD1-KVINLART)           
153101       IF WS-KVAVIS-MOT NOT = WS-DIFFERENS                                
153201         MOVE NEJ TO INDATA-SW                                            
153301         MOVE ERR-WRONG-NUMBER TO RESP-IDMSG-ERROR                        
153401       END-IF                                                             
153501     ELSE                                                                 
153601       IF WS-SUMMA-ART-BILD > WS-RAD1-KVINLART                            
153701         IF KOLLI-KVAR                                                    
153801           MOVE NEJ TO INDATA-SW                                          
153901           MOVE ERR-ON-CASELEVEL TO RESP-IDMSG-ERROR                      
154001         ELSE                                                             
154101           MOVE NEJ TO INDATA-SW                                          
154201           MOVE ERR-WRONG-NUMBER TO RESP-IDMSG-ERROR                      
154301         END-IF                                                           
154401       END-IF                                                             
154501     END-IF                                                               
154601                                                                          
154701     IF INDATA-OK                                                         
154801       MOVE +1 TO INDX                                                    
154901       PERFORM UNTIL (INDX > REQU-KVRADER) OR INDATA-FEL                  
155001         IF RADINFO(INDX) > ZERO                                          
155103           IF (REQU-KDKLIPRI-LINE(INDX) = JA OR YES OR PRIO) AND          
155203          (REQU-FLSATS-LINE(INDX) = JA OR YES)                            
155301             MOVE MFS-ALFA-FAELT-FEL TO                                   
155403                                     RESP-KDKLIPRI-LINE-ATTR(INDX)        
155503                                     RESP-FLSATS-LINE-ATTR(INDX)          
155601             MOVE NEJ TO INDATA-SW                                        
155701             MOVE ERR-KIT-PRIO TO RESP-IDMSG-ERROR                        
155801           END-IF                                                         
155901*****FÖRSTA RADEN BESTÄMMER MÄRKNING                                      
156001           IF FOERSTA-IFYLLD                                              
156103             IF REQU-KDKLIPRI-LINE(INDX) = JA OR YES OR PRIO              
156201               MOVE JA TO PRIO-SW                                         
156301             END-IF                                                       
156403             IF REQU-FLSATS-LINE(INDX) = JA OR YES                        
156501               MOVE JA TO SATS-SW                                         
156601             END-IF                                                       
156701             MOVE NEJ TO FOERSTA-SW                                       
156801           END-IF                                                         
156901           IF INDATA-OK                                                   
157003             IF (PRIOMRK AND REQU-KDKLIPRI-LINE(INDX) = NEJ) OR           
157103                (EJ-PRIO AND REQU-KDKLIPRI-LINE(INDX) = (JA OR YES        
157201                                                  OR  PRIO))              
157301               MOVE MFS-ALFA-FAELT-FEL                                    
157403               TO RESP-KDKLIPRI-LINE-ATTR(INDX)                           
157501               MOVE NEJ TO INDATA-SW                                      
157601               MOVE ERR-FLAG-LABEL-TYPE TO RESP-IDMSG-ERROR               
157701             END-IF                                                       
157801           END-IF                                                         
157901           IF INDATA-OK                                                   
158003             IF (SATS AND REQU-FLSATS-LINE(INDX) = NEJ) OR                
158103               (EJ-SATS AND REQU-FLSATS-LINE(INDX) = (JA OR YES ))        
158203               MOVE MFS-ALFA-FAELT-FEL TO                                 
158303                                       RESP-FLSATS-LINE-ATTR(INDX)        
158401               MOVE NEJ TO INDATA-SW                                      
158501               MOVE ERR-FLAG-LABEL-TYPE TO RESP-IDMSG-ERROR               
158601             END-IF                                                       
158701           END-IF                                                         
158801           IF INDATA-OK                                                   
158903             IF REQU-FLSATS-LINE(INDX) = JA OR YES                        
159003               IF REQU-KDFLETI-LINE(INDX) = ETIKETT OR MLABEL             
159101                 MOVE MFS-ALFA-FAELT-FEL                                  
159203                 TO RESP-FLSATS-LINE-ATTR(INDX)                           
159301                 MOVE NEJ TO INDATA-SW                                    
159401                 MOVE ERR-DIV-CASE TO RESP-IDMSG-ERROR                    
159501               END-IF                                                     
159603               IF REQU-FLPREPKL-LINE(INDX) = JA OR YES                    
159701                 MOVE MFS-ALFA-FAELT-FEL TO                               
159802                 RESP-FLPREPKL-LINE-ATTR(INDX)                            
159901                 MOVE NEJ TO INDATA-SW                                    
160001                 MOVE ERR-KIT-CASE TO RESP-IDMSG-ERROR                    
160101               END-IF                                                     
160201             END-IF                                                       
160301           END-IF                                                         
160401         END-IF                                                           
160501         ADD +1 TO INDX                                                   
160601       END-PERFORM                                                        
160701     END-IF                                                               
160801                                                                          
160901     IF INDATA-OK                                                         
161001       IF SATS AND PRIOMRK                                                
161103         MOVE MFS-ALFA-FAELT-FEL TO RESP-KDKLIPRI-LINE-ATTR(INDX)         
161203                                    RESP-FLSATS-LINE-ATTR(INDX)           
161301         MOVE NEJ TO INDATA-SW                                            
161401         MOVE ERR-KIT-PRIO TO RESP-IDMSG-ERROR                            
161501       END-IF                                                             
161601     END-IF                                                               
161701                                                                          
161801     .                                                                    
161901     EJECT                                                                
162001 GCC-KOLLA-REGLER-AVVIK SECTION.                                          
162101                                                                          
162201     IF WS-INLA-ART-KDRT = 3 OR 8 OR 77                                   
162301       MOVE ERR-NOT-3-OR-77 TO RESP-IDMSG-ERROR                           
162401       MOVE NEJ TO INDATA-SW                                              
162501     END-IF                                                               
162601                                                                          
162701     IF WS-INLA-ART-FLSPLPART = JA AND MFS-UPDATE                         
162801       MOVE SPLITT-PRESS-PF23   TO RESP-IDMSG-ERROR                       
162901       MOVE NEJ                 TO INDATA-SW                              
163001     END-IF                                                               
163101                                                                          
163201     IF INDATA-OK                                                         
163301       IF INLA-RAD-IDRADNR = 1                                            
163401         PERFORM IMS-GNP-OKVAL-INLA-W6D121                                
163501       END-IF                                                             
163601       PERFORM UNTIL SEGMENT-SAKNAS                                       
163701         IF INLA-RAD-KDINLSTA = SPACE AND INLA-RAD-FLSATS = NEJ           
163801           AND INLA-RAD-ADINLOMR NOT = 'SEK'                              
163901           MOVE MFS-NUM-FAELT-FEL TO RESP-IDANSTNR-ATTR                   
164001                                     RESP-KVAVIS-MOT-ATTR                 
164101           MOVE NEJ TO INDATA-SW                                          
164201           MOVE ERR-UNPACKED-CASE TO RESP-IDMSG-ERROR                     
164301         END-IF                                                           
164401         IF INLA-RAD-ADINLOMR = 'SEK'                                     
164501           ADD INLA-RAD-KVINLART TO WS-KVINLART-SEK                       
164601         END-IF                                                           
164701         PERFORM IMS-GNP-OKVAL-INLA-W6D121                                
164801       END-PERFORM                                                        
164901                                                                          
165001       IF INDATA-OK                                                       
165101         IF (NOT FPK-PARTI)  OR OMPACKNING                                
165201           MOVE MFS-NUM-FAELT-FEL TO RESP-IDANSTNR-ATTR                   
165301                                     RESP-KVAVIS-MOT-ATTR                 
165401           MOVE NEJ TO INDATA-SW                                          
165501           MOVE ERR-DEVIATION-6133 TO RESP-IDMSG-ERROR                    
165601         END-IF                                                           
165701       END-IF                                                             
165801                                                                          
165901       IF INDATA-OK                                                       
166001         IF NOT RADER-IFYLLDA                                             
166101           COMPUTE WS-DIFFERENS = WS-KVAVIS-VERKL -                       
166201                                  WS-RAD1-KVINLART                        
166301*                               - WS-KVINLART-SEK                         
166401           IF WS-DIFFERENS NOT = WS-KVAVIS-MOT                            
166501             MOVE MFS-NUM-FAELT-FEL TO RESP-KVAVIS-MOT-ATTR               
166601             MOVE NEJ TO INDATA-SW                                        
166701             MOVE ERR-WRONG-NUMBER TO RESP-IDMSG-ERROR                    
166801           END-IF                                                         
166901         END-IF                                                           
167001       END-IF                                                             
167101     END-IF                                                               
167201                                                                          
167301     .                                                                    
167401     EJECT                                                                
167501 H-UPPDATERA SECTION.                                                     
167601                                                                          
167701     IF REQU-FLPREPRA = JA OR YES                                         
167702       IF REQU-IDMSGVER = 1                                               
167703**       FOR WEB W6019E WILL PRINT THE PRE TREATMENT REPORT               
167704         MOVE WS-INLA-ART-IDLOPNRM      TO W-IDLOPNRM                     
167705         PERFORM IMS-GU-INLC-W6D1B                                        
167706         MOVE INLC-SEQB-IDDC            TO RESP-IDDC                      
167707         MOVE INLC-SEQB-IDLEVNR         TO RESP-IDLEVNR                   
167708         MOVE INLC-SEQB-IDFS            TO RESP-IDFS                      
167709         MOVE INLC-SEQB-TIAVIDAT        TO RESP-TIAVIDAT                  
167710         MOVE WS-INLA-ART-IDRADNR-INL   TO RESP-IDRADNR-INL               
167720       ELSE                                                               
167801         PERFORM S04-TRANS-TILL-6197                                      
167901       END-IF                                                             
167902     END-IF                                                               
168001     PERFORM IMS-GU-INLA-W6D111                                           
168101     IF SEGMENT-FINNS                                                     
168201       MOVE DLI-IO-AREA TO DLI-WS-AREA                                    
168301       IF RADER-IFYLLDA                                                   
168401         PERFORM HA-UPPDATERA-RADER                                       
168501       END-IF                                                             
168601       IF AVVIK-IFYLLD                                                    
168701         PERFORM HB-UPPDATERA-AVVIK                                       
168801         PERFORM HC-KOLLA-ANTALSKONTROLL                                  
168901       END-IF                                                             
169001       PERFORM HD-KOLLA-TRANSAR                                           
169101       MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                            
169201       PERFORM MFS-FORM-ATTR                                              
169301       PERFORM MFS-RENSA-FAELT-IN                                         
169401     END-IF                                                               
169501                                                                          
169601     .                                                                    
169701     EJECT                                                                
169801 HA-UPPDATERA-RADER SECTION.                                              
169901                                                                          
170001     PERFORM IMS-GHU-LOPA-W6G110                                          
170101     MOVE +1 TO INDX                                                      
170201     PERFORM UNTIL INDX > REQU-KVRADER                                    
170301       IF RADINFO(INDX) > ZERO                                            
170401         MOVE +1 TO W-IDRADNR                                             
170501         PERFORM IMS-GHNP-FIRST-INLA-W6D121                               
170601         IF SEGMENT-FINNS                                                 
170701           MOVE INLA-RAD-FLPRIO TO SPAR-FLPRIO                            
170801           MOVE INLA-RAD-KDINLPRIO TO SPAR-KDINLPRIO                      
170901           MOVE INLA-RAD-FLINLFB   TO SPAR-FLINLFB                        
171001           MOVE INLA-RAD-FLKVAANT  TO SPAR-FLKVAANT                       
171101           IF INLA-RAD-ADINLOMR NOT = SPACE                               
171201             MOVE INLA-RAD-ADINLOMR TO SPAR-ADINLOMR                      
171301           ELSE                                                           
171401             MOVE REQU-ADINLOMR-PRT TO SPAR-ADINLOMR                      
171501           END-IF                                                         
171601           MOVE INLA-RAD-KVINLART TO WS-TRANS-KVINLART-OLD                
171701           COMPUTE INLA-RAD-KVINLART = INLA-RAD-KVINLART -                
171801           (WS-KVFLETI(INDX) * WS-KVINLART(INDX))                         
171901           IF INLA-RAD-KVINLART > ZERO                                    
172001             PERFORM IMS-REPL-INLA-W6D1                                   
172101           ELSE                                                           
172201             PERFORM IMS-DLET-INLA-W6D1                                   
172301           END-IF                                                         
172401           PERFORM S01-TRANS-TILL-6191                                    
172501         ELSE                                                             
172601           MOVE REQU-ADINLOMR-PRT TO SPAR-ADINLOMR                        
172701           COMPUTE INLA-RAD-KVINLART =                                    
172801           WS-KVFLETI(INDX) * WS-KVINLART(INDX)                           
172901         END-IF                                                           
173001                                                                          
173101         PERFORM IMS-GNP-LAST-INLA-W6D121                                 
173201         MOVE +1 TO KVFLETI-RAKN                                          
173301         PERFORM UNTIL KVFLETI-RAKN > WS-KVFLETI(INDX)                    
173401           MOVE SPACE TO WS-TRANS-ADINLOMR-OLD                            
173501           MOVE SPACE TO WS-TRANS-ADINLOMR-NXT-OLD                        
173601           MOVE SPACE TO WS-TRANS-KDINLSTA-OLD                            
173701           MOVE ZERO TO WS-TRANS-KVINLART-OLD                             
173801           COMPUTE INLA-RAD-IDRADNR = INLA-RAD-IDRADNR + 1                
173901           MOVE WS-KVINLART(INDX)     TO INLA-RAD-KVINLART                
174001                                                                          
174103           IF REQU-KDFLETI-LINE(INDX) = FLAGGA OR LAB                     
174201             MOVE SPAR-IDLEVNR        TO INLA-RAD-IDLEVNR-KOLLI           
174202             IF NDC-CN OR NDC-US                                          
174203               MOVE W-IDLEVNR-CN-US   TO INLA-RAD-IDLEVNR-KOLLI           
174204             END-IF                                                       
174301             ADD +1 TO LOPA-6018-IDOKOLLI                                 
174401             MOVE LOPA-6018-IDOKOLLI  TO INLA-RAD-IDOKOLLI                
174501           ELSE                                                           
174601             MOVE SPACE               TO INLA-RAD-IDLEVNR-KOLLI           
174701             MOVE ZERO                TO INLA-RAD-IDOKOLLI                
174801           END-IF                                                         
174901                                                                          
175001           MOVE SPAR-FLINLFB          TO INLA-RAD-FLINLFB                 
175101           MOVE NEJ                   TO INLA-RAD-FLINLFP                 
175201                                         INLA-RAD-FLDIVKLI                
175301                                         INLA-RAD-FLSVSLS                 
175401                                                                          
175503           IF REQU-FLPREPKL-LINE(INDX) = JA OR YES                        
175601             MOVE 'FPK'               TO INLA-RAD-KDINLSTA                
175701             MOVE JA                  TO INLA-RAD-FLINLFP                 
175801           ELSE                                                           
175901             MOVE SPACE               TO INLA-RAD-KDINLSTA                
176001           END-IF                                                         
176101           MOVE ZERO                  TO INLA-RAD-IDILIST                 
176201                                         INLA-RAD-TIUPPDAT                
176301                                         INLA-RAD-IDINLVGN                
176401                                         INLA-RAD-IDILIRAD                
176501                                         INLA-RAD-IDANSTNR                
176601           IF OMPACKNING OR                                               
176701             (NOT FPK-PARTI) OR                                           
176801             WS-INLA-ART-KDKVAANT = ZERO                                  
176901             MOVE SPAR-FLKVAANT       TO INLA-RAD-FLKVAANT                
177001           ELSE                                                           
177101             MOVE JA                  TO INLA-RAD-FLKVAANT                
177201           END-IF                                                         
177301                                                                          
177401           MOVE SPACE                 TO INLA-RAD-ADINLOMR-NXT            
177501           MOVE SPAR-ADINLOMR         TO INLA-RAD-ADINLOMR                
177601                                                                          
177703           IF REQU-FLSATS-LINE(INDX) = JA OR YES                          
177801             MOVE JA                  TO INLA-RAD-FLSATS                  
177901                                         INLA-RAD-FLINLFP                 
178001           ELSE                                                           
178101             MOVE NEJ                 TO INLA-RAD-FLSATS                  
178201           END-IF                                                         
178301                                                                          
178401           IF SPAR-FLPRIO NOT = SPACE                                     
178501*******DVS OM IDRADNR = 1 FINNS                                           
178601             MOVE SPAR-KDINLPRIO TO INLA-RAD-KDINLPRIO                    
178701             MOVE SPAR-FLPRIO    TO INLA-RAD-FLPRIO                       
178801             PERFORM IMS-ISRT-INLA-W6D121                                 
178901             IF (SPAR-FLPRIO = JA AND                                     
179003                REQU-KDKLIPRI-LINE(INDX) = NEJ) OR                        
179101                (SPAR-FLPRIO = NEJ AND                                    
179203                (REQU-KDKLIPRI-LINE(INDX) = JA OR YES OR PRIO))           
179301*******OM PRIO PÅ BILDEN EJ STÄMMER MED IDRADNR = 1                       
179401               PERFORM HAA-ANROPA-PMRK                                    
179501             END-IF                                                       
179601           ELSE                                                           
179701             MOVE WS-INLA-ART-KDINLPRIO TO INLA-RAD-KDINLPRIO             
179801*******DET SKA VARA TVÄRTOM HÄR - W611PMRK FIXAR PRION SEDAN              
179901*          ***                                                            
180003             IF REQU-KDKLIPRI-LINE(INDX) = JA OR PRIO OR YES              
180101               MOVE NEJ TO INLA-RAD-FLPRIO                                
180201             ELSE                                                         
180301               MOVE JA TO INLA-RAD-FLPRIO                                 
180401             END-IF                                                       
180501             PERFORM IMS-ISRT-INLA-W6D121                                 
180601             PERFORM HAA-ANROPA-PMRK                                      
180701           END-IF                                                         
180801                                                                          
180901           PERFORM S01-TRANS-TILL-6191                                    
181003           IF REQU-KDFLETI-LINE(INDX) = FLAGGA OR LAB                     
181101             PERFORM S02-TRANS-TILL-6194                                  
181102             IF NDC-CN OR NDC-US                                          
181112               MOVE 6194-MID-VKKOLLIN(1)   TO RESP-VKKOLLIN               
181113               MOVE W-IDLEVNR-CN-US        TO RESP-IDLEVNR-KOLLI          
181115               MOVE 6194-MID-IDOKOLLI(1)   TO RESP-IDOKOLLI               
181117               MOVE 6194-MID-TIINLMOT(1)   TO RESP-TIINLMOT               
181120             END-IF                                                       
181201           ELSE                                                           
181301             PERFORM S03-TRANS-TILL-6195                                  
181401           END-IF                                                         
181501                                                                          
181601           ADD +1 TO KVFLETI-RAKN                                         
181701         END-PERFORM                                                      
181801       END-IF                                                             
181901       ADD +1 TO INDX                                                     
182001     END-PERFORM                                                          
182101     PERFORM IMS-REPL-LOPA-W6G1                                           
182301     .                                                                    
182401     EJECT                                                                
182501 HAA-ANROPA-PMRK SECTION.                                                 
182801     MOVE INLA-RAD-IDLEVNR-KOLLI TO PMRK-IDLEVNR                          
182901     MOVE INLA-RAD-IDOKOLLI      TO PMRK-IDOKOLLI                         
183001     MOVE ZERO                   TO PMRK-IDLOPNRM                         
183101                                    PMRK-IDRADNR                          
183201     CALL W611PMRK USING PMRK-W611PMRK PMRK-INLA1-PCB                     
183301                                       PMRK-INLA2-PCB                     
183401                                       PMRK-PLAA-PCB                      
183601     .                                                                    
183701     EJECT                                                                
183801 HB-UPPDATERA-AVVIK SECTION.                                              
184001     MOVE SPACE TO SPAR-FLPRIO                                            
184101     MOVE +1 TO W-IDRADNR                                                 
184201     PERFORM IMS-GHNP-FIRST-INLA-W6D121                                   
184301     IF SEGMENT-FINNS                                                     
184401       MOVE INLA-RAD-FLPRIO   TO SPAR-FLPRIO                              
184501       MOVE INLA-RAD-KVINLART TO WS-RAD1-KVINLART                         
184601       PERFORM IMS-DLET-INLA-W6D1                                         
184701     END-IF                                                               
184801     PERFORM IMS-GNP-LAST-INLA-W6D121                                     
184901     COMPUTE INLA-RAD-IDRADNR = INLA-RAD-IDRADNR + 1                      
185001     COMPUTE INLA-RAD-KVINLART =                                          
185101     WS-KVAVIS-VERKL - WS-KVAVIS-MOT                                      
185201                                                                          
185301     MOVE SPACE                 TO INLA-RAD-IDLEVNR-KOLLI                 
185401     MOVE ZERO                  TO INLA-RAD-IDOKOLLI                      
185501     MOVE +39                   TO INLA-RAD-KDINLPRIO                     
185601                                                                          
185701     IF WS-INLA-ART-KDKVAANT > ZERO                                       
185801       MOVE 'ANT'               TO INLA-RAD-KDINLSTA                      
185901     ELSE                                                                 
186001       MOVE 'AVV'               TO INLA-RAD-KDINLSTA                      
186101     END-IF                                                               
186201                                                                          
186301     MOVE ZERO                  TO INLA-RAD-IDILIST                       
186401                                   INLA-RAD-IDINLVGN                      
186501                                   INLA-RAD-IDILIRAD                      
186601     ACCEPT INLA-RAD-TIUPPDAT FROM DATE                                   
186701     MOVE REQU-IDANSTNR         TO INLA-RAD-IDANSTNR                      
186801     MOVE NEJ                   TO INLA-RAD-FLKVAANT                      
186901                                                                          
187001     MOVE SPACE                 TO INLA-RAD-ADINLOMR-NXT                  
187101                                   INLA-RAD-ADINLOMR                      
187201     MOVE NEJ                   TO INLA-RAD-FLINLFB                       
187301                                   INLA-RAD-FLINLFP                       
187401                                   INLA-RAD-FLDIVKLI                      
187501                                   INLA-RAD-FLSATS                        
187601                                   INLA-RAD-FLPRIO                        
187701                                   INLA-RAD-FLSVSLS                       
187801     IF INLA-RAD-KVINLART NOT = ZERO                                      
187901       PERFORM IMS-ISRT-INLA-W6D121                                       
188001                                                                          
188101       IF SPAR-FLPRIO = JA                                                
188201         PERFORM HAA-ANROPA-PMRK                                          
188301       END-IF                                                             
188401                                                                          
188501       PERFORM S01-TRANS-TILL-6191                                        
188601       PERFORM HBA-TRANS-TILL-6193                                        
188701     END-IF                                                               
188901     .                                                                    
189001     EJECT                                                                
189101 HBA-TRANS-TILL-6193 SECTION.                                             
189301     MOVE WS-IDLOPNRM TO W-IDLOPNRM                                       
189401     PERFORM IMS-GU-INLA-W6D111                                           
189501     PERFORM IMS-GNP-OKVAL-INLA-W6D121                                    
189601     PERFORM UNTIL SEGMENT-SAKNAS                                         
189701       IF INLA-RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE                     
189801         MOVE JA TO 6193-SW                                               
189901       END-IF                                                             
190001       PERFORM IMS-GNP-OKVAL-INLA-W6D121                                  
190101     END-PERFORM                                                          
190201     IF 6193-TRANS                                                        
190301*****SKICKA 6193 VIA DISPATCHER                                           
190401       MOVE WS-IDLOPNRM      TO 6193-MID-IDLOPNRM                         
190501       MOVE INLA-RAD-IDRADNR TO 6193-MID-IDRADNR                          
190601                                                                          
190701       MOVE SPACE                TO MSG-KOM-WMSGKOM                       
190801       COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                   
190901       MOVE LOW-VALUE            TO MSG-KOM-KDZ1                          
191001       MOVE LOW-VALUE            TO MSG-KOM-KDZ2                          
191101       MOVE SPACE                TO MSG-KOM-KDTRANS                       
191201       MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                      
191301       MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                      
191401       MOVE 'W6013600'           TO MSG-KOM-IDSNDJOB                      
191501       ACCEPT MSG-KOM-TIREGDAT FROM DATE                                  
191601       ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                  
191701       MOVE SPACE                TO MSG-KOM-IDMFSMED                      
191801                                                                          
191901       MOVE +29                  TO P-TO-P-MSG-KVLL                       
192001*****CTEXTLÄNGD + KOM-MSG-AREA => 12 + 17                                 
192101       MOVE 'W6T193X '           TO P-TO-P-MSG-KDTRANS                    
192201       MOVE '6136'               TO P-TO-P-MSG-IDTRANS                    
192301       IF REQU-IDSPRAK = 'SV'                                             
192401          MOVE '1'               TO P-TO-P-MSG-KDMFSFOR                   
192501       ELSE                                                               
192601          MOVE '2'               TO P-TO-P-MSG-KDMFSFOR                   
192701       END-IF                                                             
192801       MOVE MID-6193-AREA        TO P-TO-P-MSG-INDATA                     
192901                                                                          
193001       CALL W006KOM USING MSG-PCB                                         
193101                          DISP-PCB                                        
193201                          KOM-KOMA-PCB                                    
193301                          MSG-KOM-WMSGKOM                                 
193401                          P-TO-P-MSG-IO-AREA-SNUF                         
193501                                                                          
193601     END-IF                                                               
193701                                                                          
193801     .                                                                    
193901     EJECT                                                                
194001 HC-KOLLA-ANTALSKONTROLL SECTION.                                         
194101                                                                          
194201     MOVE ZERO TO WS-KVAVIS-VERKL                                         
194301     IF NOT OMPACKNING                                                    
194401       MOVE WS-IDLOPNRM TO W-IDLOPNRM                                     
194501       PERFORM IMS-GU-INLA-W6D111                                         
194601       PERFORM IMS-GNP-OKVAL-INLA-W6D121                                  
194701       PERFORM UNTIL SEGMENT-SAKNAS                                       
194801         IF INLA-RAD-KDINLSTA = 'SAK'                                     
194901           MOVE NEJ TO ANTALSKNTR-SW                                      
195001         END-IF                                                           
195101         IF INLA-RAD-KDINLSTA = 'ANT' OR 'AVV'                            
195201           COMPUTE WS-KVAANT = WS-KVAANT + INLA-RAD-KVINLART              
195301         END-IF                                                           
195401         IF INLA-RAD-KDINLSTA = 'FPK' OR 'SAK' OR 'INL' OR 'VOR'          
195501                                OR SPACE OR 'KVA' OR 'RET'                
195601                                OR 'FRD'                                  
195701           ADD INLA-RAD-KVINLART TO WS-KVAVIS-VERKL                       
195801         END-IF                                                           
195901         PERFORM IMS-GNP-OKVAL-INLA-W6D121                                
196001       END-PERFORM                                                        
196101                                                                          
196201       IF ANTALSKNTR-KLAR                                                 
196301         IF WS-KVAANT NOT = 0                                             
196401           PERFORM S05A-KOLLA-OM-TRANS-R6202                              
196501           IF R6202-TRANS                                                 
196601             PERFORM S05B-TRANS-TILL-6202                                 
196701           END-IF                                                         
196801         ELSE                                                             
196901           IF INLA-ART-KDKVAANT > 0                                       
197001             MOVE LOW-VALUE            TO W-W6H7BSEQ-MIN-X                
197101             MOVE HIGH-VALUE           TO W-W6H7BSEQ-MAX-X                
197201             MOVE WS-INLA-ART-IDLOPNRM TO W-IDLOPNRM                      
197301             MOVE WS-INLA-ART-IDARTNR  TO W-IDARTNR-H7-MIN                
197401                                          W-IDARTNR-H7-MAX                
197501             MOVE NEJ          TO TRAFF-SW                                
197601                                                                          
197701             PERFORM IMS-GU-INLC-W6D1B                                    
197801             MOVE INLC-SEQB-IDLEVNR TO W-IDLEVNR-H7-MIN                   
197901                                       W-IDLEVNR-H7-MAX                   
198001             MOVE +1                TO W-KVKRKNTR-H7-MIN                  
198101             MOVE +9                TO W-KVKRKNTR-H7-MAX                  
198201             PERFORM IMS-GHU-KVABSEQ-W6H701                               
198301             PERFORM UNTIL TRAFF OR SEGMENT-SAKNAS                        
198401               IF SEGMENT-FINNS                                           
198501                   IF KVAE-KR-IDKRFEL  = 'PA' OR 'PB' OR 'K '             
198601                       IF KVAE-KR-KVKRKNTR > ZERO                         
198701                           SUBTRACT 1      FROM KVAE-KR-KVKRKNTR          
198801                           PERFORM IMS-REPL-KVABSEQ-W6H7                  
198901                           MOVE JA TO TRAFF-SW                            
199001                       END-IF                                             
199101                   END-IF                                                 
199201               END-IF                                                     
199301               PERFORM IMS-GHN-KVABSEQ-W6H701                             
199401             END-PERFORM                                                  
199501           END-IF                                                         
199601         END-IF                                                           
199701       END-IF                                                             
199801     END-IF                                                               
199901     .                                                                    
200001     EJECT                                                                
200101 HD-KOLLA-TRANSAR SECTION.                                                
200201                                                                          
200301     IF 6191-IX > ZERO                                                    
200401       PERFORM S01A-SKICKA-TRANS-TILL-6191                                
200501     END-IF                                                               
200601     IF 6194-IX > ZERO                                                    
200602       IF REQU-IDMSGVER = 1                                               
200603         CONTINUE                                                         
200609       ELSE                                                               
200701         PERFORM S02A-SKICKA-TRANS-TILL-6194                              
200801       END-IF                                                             
200802     END-IF                                                               
200901     IF 6195-IX > ZERO                                                    
201001       PERFORM S03A-SKICKA-TRANS-TILL-6195                                
201101     END-IF                                                               
201201                                                                          
201301     .                                                                    
201401     EJECT                                                                
201501 S01-TRANS-TILL-6191 SECTION.                                             
201601                                                                          
201701     IF 6191-IX = ZERO                                                    
201801       MOVE 'W6013600'              TO 6191-MID-IDPGM                     
201901       MOVE DCS-IDDC                TO 6191-MID-IDDC                      
202001       MOVE +1                      TO 6191-MID-KVPOST                    
202101                                       6191-IX                            
202201     END-IF                                                               
202301     MOVE WS-INLA-ART-IDLOPNRM      TO 6191-MID-IDLOPNRM(6191-IX)         
202401     MOVE INLA-RAD-IDRADNR          TO 6191-MID-IDRADNR(6191-IX)          
202501     MOVE INLA-RAD-KDINLPRIO        TO 6191-MID-KDINLPRIO(6191-IX)        
202601     MOVE WS-INLA-ART-PRARTSTD      TO 6191-MID-PRARTSTD(6191-IX)         
202701     IF INLA-RAD-IDRADNR = +1                                             
202801       MOVE WS-KVFLETI(INDX)        TO 6191-MID-KVKOLLI (6191-IX)         
202901     ELSE                                                                 
203001       MOVE +0                      TO 6191-MID-KVKOLLI (6191-IX)         
203101     END-IF                                                               
203201     MOVE 'N'                       TO 6191-MID-FLINLI  (6191-IX)         
203301     MOVE WS-TRANS-ADINLOMR-OLD     TO                                    
203401     6191-MID-ADINLOMR-OLD(6191-IX)                                       
203501     MOVE WS-TRANS-ADINLOMR-NXT-OLD TO                                    
203601     6191-MID-ADINLOMR-NXT-OLD(6191-IX)                                   
203701     MOVE WS-TRANS-KDINLSTA-OLD     TO                                    
203801     6191-MID-KDINLSTA-OLD(6191-IX)                                       
203901     MOVE WS-TRANS-KVINLART-OLD     TO                                    
204001     6191-MID-KVINLART-OLD(6191-IX)                                       
204101     MOVE INLA-RAD-ADINLOMR         TO                                    
204201     6191-MID-ADINLOMR-NEW(6191-IX)                                       
204301     MOVE INLA-RAD-ADINLOMR-NXT     TO                                    
204401     6191-MID-ADINLOMR-NXT-NEW(6191-IX)                                   
204501     MOVE INLA-RAD-KDINLSTA         TO                                    
204601     6191-MID-KDINLSTA-NEW(6191-IX)                                       
204701     MOVE INLA-RAD-KVINLART         TO                                    
204801     6191-MID-KVINLART-NEW(6191-IX)                                       
204901                                                                          
205001     ADD  +1   TO 6191-MID-KVPOST                                         
205101                  6191-IX                                                 
205201                                                                          
205301     IF 6191-MID-KVPOST > 24                                              
205401       PERFORM S01A-SKICKA-TRANS-TILL-6191                                
205501     END-IF                                                               
205601                                                                          
205701     .                                                                    
205801     EJECT                                                                
205901 S01A-SKICKA-TRANS-TILL-6191 SECTION.                                     
206001                                                                          
206101       COMPUTE 6191-MID-KVPOST = 6191-IX - 1                              
206201       COMPUTE P-TO-P-MSG-KVLL    =  LNG-P-TO-P-PREFIX +                  
206301                                     17 + (6191-MID-KVPOST * 64)          
206401       MOVE 'W6T191X '            TO P-TO-P-MSG-KDTRANS                   
206501       MOVE '6136'                TO P-TO-P-MSG-IDTRANS                   
206601       IF REQU-IDSPRAK = 'SV'                                             
206701          MOVE '1'               TO P-TO-P-MSG-KDMFSFOR                   
206801       ELSE                                                               
206901          MOVE '2'               TO P-TO-P-MSG-KDMFSFOR                   
207001       END-IF                                                             
207101       MOVE 6191-MID-W6I19101     TO P-TO-P-MSG-INDATA                    
207201       IF FOERSTA-6191                                                    
207301         PERFORM IMS-ISRT-6191-MSG                                        
207401         MOVE NEJ TO 6191-SW                                              
207501       ELSE                                                               
207601         PERFORM IMS-PURG-6191-MSG                                        
207701       END-IF                                                             
207801       MOVE ZERO TO 6191-MID-KVPOST                                       
207901                    6191-IX                                               
208001     .                                                                    
208101     EJECT                                                                
208201 S02-TRANS-TILL-6194 SECTION.                                             
208301                                                                          
208401     IF 6194-IX = ZERO                                                    
208501       MOVE 'W6013600'              TO 6194-MID-IDPGM                     
208601       MOVE SPACE                   TO 6194-MID-IDPRTLST                  
208701       MOVE '6F'                    TO 6194-MID-IDPRTLST(1:2)             
208801       MOVE W-ADINLOMR-PRT          TO 6194-MID-IDPRTLST(3:6)             
208901       MOVE +1                      TO 6194-MID-KVPOST                    
209001                                       6194-IX                            
209101     END-IF                                                               
209201     MOVE WS-INLA-ART-IDARTNR       TO 6194-MID-IDARTNR(6194-IX)          
209301     MOVE WS-INLA-ART-IDLOPNRM      TO 6194-MID-IDLOPNRM(6194-IX)         
209401     MOVE INLA-RAD-KVINLART         TO 6194-MID-KVINLART(6194-IX)         
209501     MOVE INLA-RAD-IDLEVNR-KOLLI    TO                                    
209601     6194-MID-IDLEVNR-KOLLI(6194-IX)                                      
209701     MOVE INLA-RAD-IDOKOLLI         TO 6194-MID-IDOKOLLI(6194-IX)         
209801     MOVE WS-INLA-ART-IDLOPNRM      TO W-IDLOPNRM                         
209901     PERFORM IMS-GU-INLC-W6D1B                                            
210001     MOVE INLC-SEQB-IDLEVNR         TO W-IDLEVNR                          
210101     MOVE INLC-SEQB-IDFS            TO W-IDFS                             
210201     MOVE INLC-SEQB-TIAVIDAT        TO W-TIAVIDAT                         
210301     MOVE W-IDDC                    TO W-IDDC-101KY                       
210401     PERFORM IMS-GU-INLA-W6D101                                           
210501     MOVE INLA-INL-TIINLMOT         TO 6194-MID-TIINLMOT(6194-IX)         
210601     COMPUTE 6194-MID-VKKOLLIN(6194-IX) ROUNDED =                         
210701     (WS-INLA-ART-VKART * INLA-RAD-KVINLART) / 1000                       
210801     MOVE ZERO                      TO 6194-MID-VKKOLLIB(6194-IX)         
210901                                                                          
210906                                                                          
211001     IF (REQU-FLSVS = YES                                                 
211101     OR  REQU-FLSVS = JA)                                                 
211201       MOVE CLAG-ADLAGOMR-SVS       TO 6194-MID-ADLAGOMR(6194-IX)         
211301       MOVE CLAG-ADGANG-SVS         TO 6194-MID-ADGANG(6194-IX)           
211401       MOVE CLAG-ADPLATS-SVS        TO 6194-MID-ADPLATS(6194-IX)          
211501     ELSE                                                                 
211601       MOVE WS-INLA-ART-ADLAGOMR    TO 6194-MID-ADLAGOMR(6194-IX)         
211701       MOVE WS-INLA-ART-ADGANG      TO 6194-MID-ADGANG(6194-IX)           
211801       MOVE WS-INLA-ART-ADPLATS     TO 6194-MID-ADPLATS(6194-IX)          
211901     END-IF                                                               
212001     MOVE WS-INLA-ART-KDSORT        TO 6194-MID-KDSORT(6194-IX)           
212101     MOVE WS-INLA-ART-BEFT          TO 6194-MID-BEFT(6194-IX)             
212201                                                                          
212301     ADD  +1   TO 6194-MID-KVPOST                                         
212401                  6194-IX                                                 
212501                                                                          
212601     IF 6194-MID-KVPOST > 15                                              
212602       IF REQU-IDMSGVER = 1                                               
212608         CONTINUE                                                         
212620       ELSE                                                               
212701         PERFORM S02A-SKICKA-TRANS-TILL-6194                              
212801       END-IF                                                             
212802     END-IF                                                               
212901                                                                          
213001     .                                                                    
213101     EJECT                                                                
213201 S02A-SKICKA-TRANS-TILL-6194 SECTION.                                     
213301                                                                          
213401       COMPUTE 6194-MID-KVPOST = 6194-IX - 1                              
213501       COMPUTE P-TO-P-MSG-KVLL    =  LNG-P-TO-P-PREFIX +                  
213601                                     23 + (6194-MID-KVPOST * 67)          
213701       MOVE 'W6T194X '            TO P-TO-P-MSG-KDTRANS                   
213801       MOVE '6136'                TO P-TO-P-MSG-IDTRANS                   
213901       IF REQU-IDSPRAK = 'SV'                                             
214001          MOVE '1'                TO P-TO-P-MSG-KDMFSFOR                  
214101       ELSE                                                               
214201          MOVE '2'                TO P-TO-P-MSG-KDMFSFOR                  
214301       END-IF                                                             
214401       MOVE 6194-MID-W6I19401     TO P-TO-P-MSG-INDATA                    
214501       IF FOERSTA-6194                                                    
214601         PERFORM IMS-ISRT-6194-MSG                                        
214701         MOVE NEJ TO 6194-SW                                              
214801       ELSE                                                               
214901         PERFORM IMS-PURG-6194-MSG                                        
215001       END-IF                                                             
215101       MOVE ZERO TO 6194-MID-KVPOST                                       
215201                    6194-IX                                               
215301     .                                                                    
215401     EJECT                                                                
215501 S03-TRANS-TILL-6195 SECTION.                                             
215601                                                                          
215701     IF 6195-IX = ZERO                                                    
215801       MOVE 'W6013600'              TO 6195-MID-IDPGM                     
215901       MOVE SPACE                   TO 6195-MID-IDPRTLST                  
216001       MOVE '6E'                    TO 6195-MID-IDPRTLST(1:2)             
216101       MOVE W-ADINLOMR-PRT          TO 6195-MID-IDPRTLST(3:6)             
216201       MOVE +1                      TO 6195-MID-KVPOST                    
216301                                       6195-IX                            
216401     END-IF                                                               
216501     MOVE WS-INLA-ART-IDLOPNRM      TO 6195-MID-IDLOPNRM(6195-IX)         
216601     MOVE INLA-RAD-IDRADNR          TO 6195-MID-IDRADNR(6195-IX)          
216701     MOVE WS-INLA-ART-IDARTNR       TO 6195-MID-IDARTNR(6195-IX)          
216801     MOVE INLA-RAD-KVINLART         TO 6195-MID-KVINLART(6195-IX)         
216901     MOVE WS-INLA-ART-BEART         TO 6195-MID-BEART(6195-IX)            
217001     IF (REQU-FLSVS = YES                                                 
217101     OR  REQU-FLSVS = JA)                                                 
217201       MOVE CLAG-ADLAGOMR-SVS       TO 6195-MID-ADLAGOMR(6195-IX)         
217301       MOVE CLAG-ADGANG-SVS         TO 6195-MID-ADGANG(6195-IX)           
217401       MOVE CLAG-ADPLATS-SVS        TO 6195-MID-ADPLATS(6195-IX)          
217501     ELSE                                                                 
217601       MOVE WS-INLA-ART-ADLAGOMR    TO 6195-MID-ADLAGOMR(6195-IX)         
217701       MOVE WS-INLA-ART-ADGANG      TO 6195-MID-ADGANG(6195-IX)           
217801       MOVE WS-INLA-ART-ADPLATS     TO 6195-MID-ADPLATS(6195-IX)          
217901     END-IF                                                               
218001                                                                          
218101     ADD  +1   TO 6195-MID-KVPOST                                         
218201                  6195-IX                                                 
218301                                                                          
218401     IF 6195-MID-KVPOST > 15                                              
218501       PERFORM S03A-SKICKA-TRANS-TILL-6195                                
218601     END-IF                                                               
218701                                                                          
218801     .                                                                    
218901     EJECT                                                                
219001 S03A-SKICKA-TRANS-TILL-6195 SECTION.                                     
219101                                                                          
219201     COMPUTE 6195-MID-KVPOST = 6195-IX - 1                                
219301     COMPUTE P-TO-P-MSG-KVLL    =  LNG-P-TO-P-PREFIX +                    
219401                                   23 + (6195-MID-KVPOST * 61)            
219501     MOVE 'W6T195X '            TO P-TO-P-MSG-KDTRANS                     
219601     MOVE '6136'                TO P-TO-P-MSG-IDTRANS                     
219801     IF REQU-IDSPRAK = 'SV'                                               
219901        MOVE '1'                TO P-TO-P-MSG-KDMFSFOR                    
220001     ELSE                                                                 
220101        MOVE '2'                TO P-TO-P-MSG-KDMFSFOR                    
220201     END-IF                                                               
220301     MOVE 6195-MID-W6I19501     TO P-TO-P-MSG-INDATA                      
220401     IF FOERSTA-6195                                                      
220501       PERFORM IMS-ISRT-6195-MSG                                          
220601       MOVE NEJ TO 6195-SW                                                
220701     ELSE                                                                 
220801       PERFORM IMS-PURG-6195-MSG                                          
220901     END-IF                                                               
221001     MOVE ZERO TO 6195-MID-KVPOST                                         
221101                  6195-IX                                                 
221201     .                                                                    
221301     EJECT                                                                
221401 S04-TRANS-TILL-6197 SECTION.                                             
221501                                                                          
221601     MOVE 'W6013600'              TO 6197-MID-IDPGM                       
221701     MOVE SPACE                   TO 6197-MID-IDPRTLST                    
221801     MOVE SPAR-IDPRTLST-VAL       TO 6197-MID-IDPRTLST(1:2)               
221901     MOVE W-ADINLOMR-PRT          TO 6197-MID-IDPRTLST(3:6)               
222001     MOVE +1                      TO 6197-MID-KVPOST                      
222101                                     6197-IX                              
222201     IF REQU-FLSVS = JA OR YES                                            
222301       MOVE JA                  TO 6197-MID-FLSVS                         
222401     ELSE                                                                 
222501       MOVE NEJ                 TO 6197-MID-FLSVS                         
222601     END-IF                                                               
222701     MOVE WS-INLA-ART-IDRADNR-INL TO 6197-MID-IDRADNR-INL(6197-IX)        
222801     MOVE WS-INLA-ART-IDLOPNRM      TO W-IDLOPNRM                         
222901     PERFORM IMS-GU-INLC-W6D1B                                            
223001     MOVE INLC-SEQB-IDDC            TO 6197-MID-IDDC                      
223101     MOVE INLC-SEQB-IDLEVNR         TO 6197-MID-IDLEVNR(6197-IX)          
223201     MOVE INLC-SEQB-IDFS            TO 6197-MID-IDFS(6197-IX)             
223301     MOVE INLC-SEQB-TIAVIDAT        TO 6197-MID-TIAVIDAT(6197-IX)         
223401                                                                          
223501     COMPUTE P-TO-P-MSG-KVLL     =  LNG-P-TO-P-PREFIX +                   
223601                                   26 + (6197-MID-KVPOST * 24)            
223701     MOVE 'W6T197X '            TO P-TO-P-MSG-KDTRANS                     
223801     MOVE '6136'                TO P-TO-P-MSG-IDTRANS                     
224001     IF REQU-IDSPRAK = 'SV'                                               
224101        MOVE '1'                TO P-TO-P-MSG-KDMFSFOR                    
224201     ELSE                                                                 
224301        MOVE '2'                TO P-TO-P-MSG-KDMFSFOR                    
224401     END-IF                                                               
224501     MOVE 6197-MID-W6I19701     TO P-TO-P-MSG-INDATA                      
224601     PERFORM IMS-ISRT-6197-MSG                                            
224701                                                                          
224801     .                                                                    
224901     EJECT                                                                
225001 S05A-KOLLA-OM-TRANS-R6202  SECTION.                                      
225101                                                                          
225201     MOVE JA TO R6202-SW                                                  
225301                                                                          
225401     MOVE WS-INLA-ART-IDLOPNRM      TO W-IDLOPNRM                         
225501     PERFORM IMS-GU-INLC-W6D1B                                            
225601                                                                          
225701     IF WS-INLA-ART-KDRT   = 3 OR 6 OR 7 OR 77 OR 8 OR                    
225801        WS-INLA-ART-KVAVIS = +0                                           
225901       MOVE NEJ TO R6202-SW                                               
226001     ELSE                                                                 
226101       MOVE INLC-SEQB-IDLOPNRM   TO W-IDLOPNRM-H7                         
226201       MOVE INLC-SEQB-TIAVIDAT   TO W-DAAVSDAT-H7                         
226301       IF INLC-SEQB-TIAVIDAT NOT = ZERO                                   
226401         IF INLC-SEQB-TIAVIDAT < 500000                                   
226501           MOVE 20               TO W-DAAVSDAT-H7 (1:2)                   
226601         ELSE                                                             
226701           IF INLC-SEQB-TIAVIDAT < 999999                                 
226801             MOVE 19             TO W-DAAVSDAT-H7 (1:2)                   
226901           ELSE                                                           
227001             MOVE 99999999       TO W-DAAVSDAT-H7                         
227101           END-IF                                                         
227201         END-IF                                                           
227301       END-IF                                                             
227401       PERFORM IMS-GHU-KVAE-W6H701                                        
227501       IF SEGMENT-FINNS                                                   
227601           IF KVAE-KR-IDKRFEL    = 'PA' OR 'PB' OR 'K '                   
227701             IF (KVAE-KR-KDKRSTA = '0' OR '1') OR                         
227801                (KVAE-KR-KDKRSTA > '1'        AND                         
227901                 KVAE-KR-FLKRGODK = NEJ )                                 
228001                IF KVAE-KR-FLANNULL = JA                                  
228101                  MOVE NEJ TO KVAE-KR-FLANNULL                            
228201                  PERFORM IMS-REPL-KVAE-W6H7                              
228301                END-IF                                                    
228401             ELSE                                                         
228501                MOVE NEJ                TO R6202-SW                       
228601             END-IF                                                       
228701           ELSE                                                           
228801               PERFORM IMS-GHN-KVAE-W6H701                                
228901               IF SEGMENT-FINNS                                           
229001                 IF KVAE-KR-IDKRFEL  = 'PA' OR 'PB' OR 'K '               
229101                     IF (KVAE-KR-KDKRSTA = '0' OR '1') OR                 
229201                        (KVAE-KR-KDKRSTA > '1'        AND                 
229301                         KVAE-KR-FLKRGODK = NEJ )                         
229401                       IF KVAE-KR-FLANNULL = JA                           
229501                         MOVE NEJ TO KVAE-KR-FLANNULL                     
229601                         PERFORM IMS-REPL-KVAE-W6H7                       
229701                       END-IF                                             
229801                     ELSE                                                 
229901                       MOVE NEJ                TO R6202-SW                
230001                     END-IF                                               
230101                 END-IF                                                   
230201             END-IF                                                       
230301           END-IF                                                         
230401       END-IF                                                             
230501     END-IF                                                               
230601     .                                                                    
230701     EJECT                                                                
230801 S05B-TRANS-TILL-6202 SECTION.                                            
230901                                                                          
231001     MOVE ALL '+'              TO 6202-REQU-W60202I1                      
231201     MOVE W-IDLOPNRM           TO 6202-REQU-IDLOPNRM-UPD                  
231301     MOVE INLC-SEQB-TIAVIDAT   TO 6202-REQU-TIAVSDAT-UPD                  
231401     MOVE WS-KVAVIS-VERKL      TO 6202-REQU-KVANTMOT-UPD                  
231501     MOVE REQU-IDANSTNR        TO 6202-REQU-BEKRBEH-UPD                   
231601     MOVE W-KVKRBEH            TO 6202-REQU-KVKRBEH-UPD                   
231603     MOVE ZERO                 TO 6202-REQU-IDKR-KEY                      
231604     MOVE REQU-IDDC-KEY        TO 6202-REQU-IDDC-KEY                      
231701                                                                          
231702     COMPUTE 6202-REQU-KVLL    = LENGTH OF 6202-AREA                      
231703     MOVE LOW-VALUE            TO 6202-REQU-KDZ1 6202-REQU-KDZ2           
231704     SET 6202-REQU-UPD-X       TO TRUE                                    
231705     MOVE 'W6W202T '           TO 6202-REQU-KDTRANS                       
231706     MOVE 101                  TO 6202-REQU-IDMSGVER                      
231707     MOVE REQU-IDUSER          TO 6202-REQU-IDUSER                        
231708                                                                          
232901     PERFORM IMS-ISRT-6202-MSG                                            
233001     .                                                                    
233101     EJECT                                                                
233201                                                                          
233301 S50-PRIM-CONTROL SECTION.                                                
233401* THIS IS A CONTROL TO CHECK THE OLD PLACE IF FLAG FLKNTRGK = YES         
233501* IF THE FLAG IS YES WE HAVE TO DO A CHECK ON THE OLD PLACE BEFORE        
233601* WE CHECK THE NEW PLACE.                                                 
233701     MOVE INLA-RAD-ADINLOMR    TO W-ADINLOMR                              
233801     PERFORM IMS-GU-PLAA-W6G130                                           
233901     IF SEGMENT-FINNS                                                     
234001       IF PLAA-6006-FLKNTRGK = JA                                         
234101         MOVE REQU-ADINLOMR-PRT TO W-ADINLOMR                             
234201         PERFORM IMS-GU-PLAA-W6G130                                       
234301         IF (PLAA-6006-FLKNTRGK = JA )                                    
234401         OR PLAA-6006-KDINLOMR = 'LPL'                                    
234501           MOVE 'N' TO PRIM-CONTROL-SW                                    
234601         ELSE                                                             
234701           MOVE 'J' TO PRIM-CONTROL-SW                                    
234801         END-IF                                                           
234901       ELSE                                                               
235001         MOVE 'N' TO PRIM-CONTROL-SW                                      
235101       END-IF                                                             
235201     ELSE                                                                 
235301       MOVE 'N' TO PRIM-CONTROL-SW                                        
235401     END-IF                                                               
235501     IF PRIM-CONTROL-YES                                                  
235601       PERFORM S51-CHECK-OLD-PLACE                                        
235701     END-IF                                                               
235801     .                                                                    
235901     EJECT                                                                
236001                                                                          
236101 S51-CHECK-OLD-PLACE     SECTION.                                         
236201     MOVE WS-IDLOPNRM      TO W1-IDLOPNRM                                 
236301     PERFORM IMS-GU-UPFA01                                                
236401     IF SEGMENT-FINNS                                                     
236501       IF UPPF-KVKVAPRIM > ZERO                                           
236601         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
236701           CONTINUE                                                       
236801         ELSE                                                             
236901           MOVE ERR-PRI-SPL-CTL-NOT-DONE TO RESP-IDMSG-ERROR              
237001           MOVE NEJ        TO INDATA-SW                                   
237101         END-IF                                                           
237201       END-IF                                                             
237301       IF UPPF-KVKVASEK > ZERO                                            
237401         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
237501           CONTINUE                                                       
237601         ELSE                                                             
237701           MOVE ERR-PRI-SPL-CTL-NOT-DONE TO RESP-IDMSG-ERROR              
237801           MOVE NEJ        TO INDATA-SW                                   
237901         END-IF                                                           
238001       END-IF                                                             
238101     END-IF                                                               
238201                                                                          
238301     IF INDATA-OK                                                         
238401       PERFORM IMS-GU-UPFA01                                              
238501       IF SEGMENT-FINNS                                                   
238601         PERFORM IMS-GNP-UPFA11                                           
238701         PERFORM UNTIL SEGMENT-SAKNAS                                     
238801           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
238901             CONTINUE                                                     
239001           ELSE                                                           
239101             MOVE ERR-PRI-SPL-CTL-NOT-DONE TO RESP-IDMSG-ERROR            
239201             MOVE NEJ        TO INDATA-SW                                 
239301           END-IF                                                         
239401           PERFORM IMS-GNP-UPFA11                                         
239501         END-PERFORM                                                      
239601       END-IF                                                             
239701     END-IF                                                               
239801                                                                          
239901     IF INDATA-OK                                                         
240001       PERFORM IMS-GU-UPFA01                                              
240101       IF SEGMENT-FINNS                                                   
240201         PERFORM IMS-GNP-UPFA12                                           
240301         PERFORM UNTIL SEGMENT-SAKNAS                                     
240401           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
240501             CONTINUE                                                     
240601           ELSE                                                           
240701             MOVE ERR-PRI-SPL-CTL-NOT-DONE TO RESP-IDMSG-ERROR            
240801             MOVE NEJ        TO INDATA-SW                                 
240901           END-IF                                                         
241001           PERFORM IMS-GNP-UPFA12                                         
241101         END-PERFORM                                                      
241201       END-IF                                                             
241301     END-IF                                                               
241401     .                                                                    
241501     EJECT                                                                
241601                                                                          
241701 MFS-RENSA-FAELT-UT SECTION.                                              
241801                                                                          
241901*    --- ALLA UTDATA-FÄLT                                                 
242001     MOVE ALL-SPACE       TO RESP-IDARTNR                                 
242101                             RESP-KVAVIS                                  
242201                             RESP-BEART                                   
242301                             RESP-KDSORT                                  
242401                             RESP-BEFT                                    
242501                             RESP-BEFARLIG                                
242601                             RESP-KVAVIS-KVAR                             
242701                             RESP-KVAVIS-FPK-KVAR                         
242801                             RESP-KVAVIS-PRIO-KVAR                        
242901                             RESP-KDLAGEMB                                
243001                             RESP-ADLAGOMR                                
243101                             RESP-ADGANG                                  
243201                             RESP-ADPLATS                                 
243301                             RESP-FLKVAANT-TOT                            
243401                             RESP-KVAVIS-KIT-KVAR                         
243501                             RESP-ADTRDEST-KIT                            
243601                             RESP-FLSVS                                   
243602                             RESP-KVROS                                   
243603                             RESP-FLPREPRA                                
243604                             RESP-ADINLOMR-PRT                            
243701     .                                                                    
243801     SKIP2                                                                
243901 MFS-RENSA-FAELT-IN SECTION.                                              
244001                                                                          
244101*    --- ALLA INDATA-FÄLT                                                 
244201     MOVE +1 TO INDX                                                      
244301     PERFORM UNTIL INDX > REQU-KVRADER                                    
244403       MOVE ALL-SPACE     TO   RESP-KDFLETI-LINE(INDX)                    
244503                               RESP-KVFLETI-LINE(INDX)                    
244603                               RESP-KVINLART-LINE1(INDX)                  
244701       ADD +1 TO INDX                                                     
244801     END-PERFORM                                                          
244901     MOVE ALL-SPACE       TO RESP-KVAVIS-MOT                              
245001                             RESP-IDANSTNR                                
245101                             RESP-FLSVS                                   
245301     .                                                                    
245401     EJECT                                                                
245501 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
245601                                                                          
245701*    --- ALLA UTDATA-FÄLT                                                 
245801     MOVE ALL-PLUS          TO RESP-IDARTNR                               
245901                               RESP-KVAVIS                                
246001                               RESP-BEART                                 
246101                               RESP-KDSORT                                
246201                               RESP-BEFT                                  
246301                               RESP-BEFARLIG                              
246401                               RESP-KVAVIS-KVAR                           
246501                               RESP-KVAVIS-FPK-KVAR                       
246601                               RESP-KVAVIS-PRIO-KVAR                      
246701                               RESP-KDLAGEMB                              
246801                               RESP-ADLAGOMR                              
246901                               RESP-ADGANG                                
247001                               RESP-ADPLATS                               
247101                               RESP-FLKVAANT-TOT                          
247201                               RESP-KVAVIS-KIT-KVAR                       
247301                               RESP-ADTRDEST-KIT                          
247401                               RESP-FLSVS                                 
247501     .                                                                    
247601     SKIP2                                                                
247701 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
247801                                                                          
247901*    --- ALLA INDATA-FÄLT                                                 
248001     MOVE ALL-PLUS          TO RESP-FLPREPRA                              
248101     MOVE +1 TO INDX                                                      
248201     PERFORM UNTIL INDX > REQU-KVRADER                                    
248302       MOVE ALL-PLUS        TO RESP-KDFLETI-LINE(INDX)                    
248402                               RESP-KVFLETI-LINE(INDX)                    
248502                               RESP-KVINLART-LINE1(INDX)                  
248602                               RESP-KDKLIPRI-LINE(INDX)                   
248702                               RESP-FLSATS-LINE(INDX)                     
248802                               RESP-FLPREPKL-LINE(INDX)                   
248901       ADD +1 TO INDX                                                     
249001     END-PERFORM                                                          
249101     MOVE ALL-PLUS          TO RESP-KVAVIS-MOT                            
249201                               RESP-IDANSTNR                              
249301                                                                          
249401                                                                          
249501     .                                                                    
249601     EJECT                                                                
249701 MFS-FORM-ATTR SECTION.                                                   
249801                                                                          
249901*    --- ALLA INDATA-FÄLT                                                 
250001     MOVE MFS-FORMATETS-ATTR TO RESP-FLPREPRA-ATTR                        
250101     MOVE +1 TO INDX                                                      
250201     PERFORM UNTIL INDX > REQU-KVRADER                                    
250302       MOVE MFS-FORMATETS-ATTR TO RESP-KDFLETI-LINE-ATTR(INDX)            
250402                                  RESP-KVFLETI-LINE-ATTR(INDX)            
250502                                  RESP-KVINLART-LINE1-ATTR(INDX)          
250601       ADD +1 TO INDX                                                     
250701     END-PERFORM                                                          
250801     MOVE MFS-FORMATETS-ATTR TO RESP-KVAVIS-MOT-ATTR                      
250901                                RESP-IDANSTNR-ATTR                        
251001                                                                          
251101                                                                          
251201     .                                                                    
251301     SKIP2                                                                
251401 MFS-LAES-IN-IGEN SECTION.                                                
251501                                                                          
251601*    --- ALLA INDATA-FÄLT                                                 
251701     MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLPREPRA-ATTR                     
251801     MOVE +1 TO INDX                                                      
251901     PERFORM UNTIL INDX > REQU-KVRADER                                    
252002       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KDFLETI-LINE-ATTR(INDX)         
252102                                  RESP-KVFLETI-LINE-ATTR(INDX)            
252202                                  RESP-KVINLART-LINE1-ATTR(INDX)          
252302                                  RESP-KDKLIPRI-LINE-ATTR(INDX)           
252402                                  RESP-FLSATS-LINE-ATTR(INDX)             
252502                                  RESP-FLPREPKL-LINE-ATTR(INDX)           
252601       ADD +1 TO INDX                                                     
252701     END-PERFORM                                                          
252801     MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KVAVIS-MOT-ATTR                   
252901                                   RESP-IDANSTNR-ATTR                     
253001                                                                          
253101                                                                          
253201                                                                          
253301     .                                                                    
253401     EJECT                                                                
253501* --- IMS SEKTIONER ---                                                   
253601     SKIP3                                                                
255701 IMS-ISRT-6191-MSG SECTION.                                               
255801     MOVE SPACE TO GODK-STATUSKODER                                       
255901     CALL CBLTDLI USING ISRT 6191-PCB P-TO-P-MSG-IO-AREA-SNUF             
256001     MOVE 6191-STATUS-CODE TO STATUS-WS                                   
256101     PERFORM IMS-STATUSKONTROLL                                           
256201     .                                                                    
256301     SKIP3                                                                
256401 IMS-PURG-6191-MSG SECTION.                                               
256501     MOVE SPACE TO GODK-STATUSKODER                                       
256601     CALL CBLTDLI USING PURG 6191-PCB P-TO-P-MSG-IO-AREA-SNUF             
256701     MOVE 6191-STATUS-CODE TO STATUS-WS                                   
256801     PERFORM IMS-STATUSKONTROLL                                           
256901     .                                                                    
257001     SKIP3                                                                
257101 IMS-ISRT-6194-MSG SECTION.                                               
257201     MOVE SPACE TO GODK-STATUSKODER                                       
257301     CALL CBLTDLI USING ISRT 6194-PCB P-TO-P-MSG-IO-AREA-SNUF             
257401     MOVE 6194-STATUS-CODE TO STATUS-WS                                   
257501     PERFORM IMS-STATUSKONTROLL                                           
257601     .                                                                    
257701     SKIP3                                                                
257801 IMS-PURG-6194-MSG SECTION.                                               
257901     MOVE SPACE TO GODK-STATUSKODER                                       
258001     CALL CBLTDLI USING PURG 6194-PCB P-TO-P-MSG-IO-AREA-SNUF             
258101     MOVE 6194-STATUS-CODE TO STATUS-WS                                   
258201     PERFORM IMS-STATUSKONTROLL                                           
258301     .                                                                    
258401     SKIP3                                                                
258501 IMS-ISRT-6195-MSG SECTION.                                               
258601     MOVE SPACE TO GODK-STATUSKODER                                       
258701     CALL CBLTDLI USING ISRT 6195-PCB P-TO-P-MSG-IO-AREA-SNUF             
258801     MOVE 6195-STATUS-CODE TO STATUS-WS                                   
258901     PERFORM IMS-STATUSKONTROLL                                           
259001     .                                                                    
259101     SKIP3                                                                
259201 IMS-PURG-6195-MSG SECTION.                                               
259301     MOVE SPACE TO GODK-STATUSKODER                                       
259401     CALL CBLTDLI USING PURG 6195-PCB P-TO-P-MSG-IO-AREA-SNUF             
259501     MOVE 6195-STATUS-CODE TO STATUS-WS                                   
259601     PERFORM IMS-STATUSKONTROLL                                           
259701     .                                                                    
259801     SKIP3                                                                
259901 IMS-ISRT-6197-MSG SECTION.                                               
260001     MOVE SPACE TO GODK-STATUSKODER                                       
260101     CALL CBLTDLI USING ISRT 6197-PCB P-TO-P-MSG-IO-AREA-SNUF             
260201     MOVE 6197-STATUS-CODE TO STATUS-WS                                   
260301     PERFORM IMS-STATUSKONTROLL                                           
260401     .                                                                    
260501     EJECT                                                                
260601 IMS-ISRT-6202-MSG SECTION.                                               
260701     MOVE SPACE TO GODK-STATUSKODER                                       
260801     CALL CBLTDLI USING ISRT 6202-PCB 6202-AREA                           
260901     MOVE 6202-STATUS-CODE TO STATUS-WS                                   
261001     PERFORM IMS-STATUSKONTROLL                                           
261101     .                                                                    
261201     EJECT                                                                
261301 IMS-GU-INLA-W6D101 SECTION.                                              
261401     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
261501          DELIMITED BY SIZE INTO SSA1                                     
261601     MOVE '  ' TO GODK-STATUSKODER                                        
261701     CALL CBLTDLI USING GU INLA-PCB DLI-INLA-AREA SSA1                    
261801     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
261901     PERFORM IMS-STATUSKONTROLL                                           
262001     .                                                                    
262101     SKIP3                                                                
262201 IMS-GU-INLA-W6D111 SECTION.                                              
262301     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
262401                    '&IDDC     =' W-IDDC-X ')'                            
262501          DELIMITED BY SIZE INTO SSA1                                     
262601     MOVE '  GE' TO GODK-STATUSKODER                                      
262701     CALL CBLTDLI USING GU INLABSEQ-PCB DLI-IO-AREA SSA1                  
262801     MOVE INLABSEQ-STATUS-CODE TO STATUS-WS                               
262901     PERFORM IMS-STATUSKONTROLL                                           
263001     .                                                                    
263101     SKIP3                                                                
263201 IMS-GHNP-FIRST-INLA-W6D121 SECTION.                                      
263301     STRING 'W6INLA21*F(IDRADNR  =' W-IDRADNR-X ')'                       
263401          DELIMITED BY SIZE INTO SSA1                                     
263501     MOVE '  GE' TO GODK-STATUSKODER                                      
263601     CALL CBLTDLI USING GHNP INLABSEQ-PCB DLI-IO-AREA SSA1                
263701     MOVE INLABSEQ-STATUS-CODE TO STATUS-WS                               
263801     PERFORM IMS-STATUSKONTROLL                                           
263901     .                                                                    
264001     SKIP3                                                                
264101 IMS-GNP-KVAL-INLA-W6D121 SECTION.                                        
264201     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
264301          DELIMITED BY SIZE INTO SSA1                                     
264401     MOVE '  GE' TO GODK-STATUSKODER                                      
264501     CALL CBLTDLI USING GNP INLABSEQ-PCB DLI-IO-AREA SSA1                 
264601     MOVE INLABSEQ-STATUS-CODE TO STATUS-WS                               
264701     PERFORM IMS-STATUSKONTROLL                                           
264801     .                                                                    
264901     SKIP3                                                                
265001 IMS-GNP-OKVAL-INLA-W6D121 SECTION.                                       
265101     MOVE 'W6INLA21 ' TO SSA1                                             
265201     MOVE '  GE' TO GODK-STATUSKODER                                      
265301     CALL CBLTDLI USING GNP INLABSEQ-PCB DLI-IO-AREA SSA1                 
265401     MOVE INLABSEQ-STATUS-CODE TO STATUS-WS                               
265501     PERFORM IMS-STATUSKONTROLL                                           
265601     .                                                                    
265701     SKIP3                                                                
265801 IMS-GNP-LAST-INLA-W6D121 SECTION.                                        
265901     MOVE 'W6INLA21*L' TO SSA1                                            
266001     MOVE '  GE' TO GODK-STATUSKODER                                      
266101     CALL CBLTDLI USING GNP INLABSEQ-PCB DLI-IO-AREA SSA1                 
266201     MOVE INLABSEQ-STATUS-CODE TO STATUS-WS                               
266301     PERFORM IMS-STATUSKONTROLL                                           
266401     .                                                                    
266501     SKIP3                                                                
266601 IMS-ISRT-INLA-W6D121 SECTION.                                            
266701     MOVE 'W6INLA21 ' TO SSA1                                             
266801     MOVE '  ' TO GODK-STATUSKODER                                        
266901     CALL CBLTDLI USING ISRT INLABSEQ-PCB DLI-IO-AREA SSA1                
267001     MOVE INLABSEQ-STATUS-CODE TO STATUS-WS                               
267101     PERFORM IMS-STATUSKONTROLL                                           
267201     .                                                                    
267301     SKIP3                                                                
267401 IMS-REPL-INLA-W6D1 SECTION.                                              
267501                                                                          
267601     MOVE '  ' TO GODK-STATUSKODER                                        
267701     CALL CBLTDLI USING REPL INLABSEQ-PCB DLI-IO-AREA                     
267801     MOVE INLABSEQ-STATUS-CODE TO STATUS-WS                               
267901     PERFORM IMS-STATUSKONTROLL                                           
268001     .                                                                    
268101     SKIP3                                                                
268201 IMS-DLET-INLA-W6D1 SECTION.                                              
268301                                                                          
268401     MOVE '  ' TO GODK-STATUSKODER                                        
268501     CALL CBLTDLI USING DLET INLABSEQ-PCB DLI-IO-AREA                     
268601     MOVE INLABSEQ-STATUS-CODE TO STATUS-WS                               
268701     PERFORM IMS-STATUSKONTROLL                                           
268801     .                                                                    
268901     SKIP3                                                                
269001 IMS-GU-INLC-W6D1B SECTION.                                               
269101                                                                          
269201     STRING 'W6INLC01(W6D1B1KY =' W-W6D1BSEQ-X ')'                        
269301          DELIMITED BY SIZE INTO SSA1                                     
269401     MOVE '  ' TO GODK-STATUSKODER                                        
269501     CALL CBLTDLI USING GU INLC-PCB DLI-INLC-AREA SSA1                    
269601     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
269701     PERFORM IMS-STATUSKONTROLL                                           
269801                                                                          
269901     .                                                                    
270001     EJECT                                                                
270101 IMS-GU-PLAA-W6G130 SECTION.                                              
270201     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
270301          DELIMITED BY SIZE INTO SSA1                                     
270401     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
270501          DELIMITED BY SIZE INTO SSA2                                     
270601     MOVE '  GE' TO GODK-STATUSKODER                                      
270701     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
270801     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
270901     PERFORM IMS-STATUSKONTROLL                                           
271001     .                                                                    
271101     EJECT                                                                
271201 IMS-GHU-LOPA-W6G110 SECTION.                                             
271301     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
271401          DELIMITED BY SIZE INTO SSA1                                     
271501     STRING 'W6LOPA11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
271601          DELIMITED BY SIZE INTO SSA2                                     
271701     MOVE '  ' TO GODK-STATUSKODER                                        
271801     CALL CBLTDLI USING GHU LOPA-PCB DLI-LOPA-AREA SSA1 SSA2              
271901     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
272001     PERFORM IMS-STATUSKONTROLL                                           
272101     .                                                                    
272201     SKIP2                                                                
272301 IMS-REPL-LOPA-W6G1 SECTION.                                              
272401                                                                          
272501     MOVE '  ' TO GODK-STATUSKODER                                        
272601     CALL CBLTDLI USING REPL LOPA-PCB DLI-LOPA-AREA                       
272701     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
272801     PERFORM IMS-STATUSKONTROLL                                           
272901     .                                                                    
273001     EJECT                                                                
273101 IMS-GHU-KVAE-W6H701 SECTION.                                             
273201     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
273301          DELIMITED BY SIZE INTO SSA1                                     
273401     MOVE '  GE' TO GODK-STATUSKODER                                      
273501     CALL CBLTDLI USING GHU KVAE-PCB DLI-IO-AREA SSA1                     
273601     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
273701     PERFORM IMS-STATUSKONTROLL                                           
273801     .                                                                    
273901     SKIP3                                                                
274001 IMS-GHN-KVAE-W6H701 SECTION.                                             
274101     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
274201          DELIMITED BY SIZE INTO SSA1                                     
274301     MOVE '  GE' TO GODK-STATUSKODER                                      
274401     CALL CBLTDLI USING GHN KVAE-PCB DLI-IO-AREA SSA1                     
274501     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
274601     PERFORM IMS-STATUSKONTROLL                                           
274701     .                                                                    
274801     SKIP3                                                                
274901 IMS-REPL-KVAE-W6H7 SECTION.                                              
275001                                                                          
275101     MOVE '  ' TO GODK-STATUSKODER                                        
275201     CALL CBLTDLI USING REPL KVAE-PCB DLI-IO-AREA                         
275301     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
275401     PERFORM IMS-STATUSKONTROLL                                           
275501     .                                                                    
275601     EJECT                                                                
275701 IMS-GHU-KVABSEQ-W6H701 SECTION.                                          
275801     STRING 'W6KVAE01(W6H7BSEQ>=' W-W6H7BSEQ-MIN-X                        
275901                    '&W6H7BSEQ<=' W-W6H7BSEQ-MAX-X                        
276001                    '&IDLEVNR  =' W-IDLEVNR-H7-MIN-X                      
276101                    '&KVKRKNTR=>' W-KVKRKNTR-H7-MIN-X                     
276201                    '&KVKRKNTR=<' W-KVKRKNTR-H7-MAX-X ')'                 
276301          DELIMITED BY SIZE INTO SSA1                                     
276401     MOVE '  GE' TO GODK-STATUSKODER                                      
276501     CALL CBLTDLI USING GHU KVABSEQ-PCB DLI-IO-AREA SSA1                  
276601     MOVE KVABSEQ-STATUS-CODE TO STATUS-WS                                
276701     PERFORM IMS-STATUSKONTROLL                                           
276801     .                                                                    
276901     SKIP3                                                                
277001 IMS-GHN-KVABSEQ-W6H701 SECTION.                                          
277101     STRING 'W6KVAE01(W6H7BSEQ>=' W-W6H7BSEQ-MIN-X                        
277201                    '&W6H7BSEQ<=' W-W6H7BSEQ-MAX-X                        
277301                    '&IDLEVNR  =' W-IDLEVNR-H7-MIN-X                      
277401                    '&KVKRKNTR=>' W-KVKRKNTR-H7-MIN-X                     
277501                    '&KVKRKNTR=<' W-KVKRKNTR-H7-MAX-X ')'                 
277601          DELIMITED BY SIZE INTO SSA1                                     
277701     MOVE '  GEGB' TO GODK-STATUSKODER                                    
277801     CALL CBLTDLI USING GHN KVABSEQ-PCB DLI-IO-AREA SSA1                  
277901     MOVE KVABSEQ-STATUS-CODE TO STATUS-WS                                
278001     PERFORM IMS-STATUSKONTROLL                                           
278101     .                                                                    
278201     SKIP3                                                                
278301 IMS-REPL-KVABSEQ-W6H7 SECTION.                                           
278401                                                                          
278501     MOVE '  ' TO GODK-STATUSKODER                                        
278601     CALL CBLTDLI USING REPL KVABSEQ-PCB DLI-IO-AREA                      
278701     MOVE KVABSEQ-STATUS-CODE TO STATUS-WS                                
278801     PERFORM IMS-STATUSKONTROLL                                           
278901     .                                                                    
279001     EJECT                                                                
279101 IMS-GU-WDK611 SECTION.                                                   
279201     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
279301          DELIMITED BY SIZE INTO SSA1                                     
279401     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY ')'                          
279501          DELIMITED BY SIZE INTO SSA2                                     
279601     MOVE '  GE' TO GODK-STATUSKODER                                      
279701     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK6 SSA1 SSA2            
279801     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
279901     PERFORM IMS-STATUSKONTROLL                                           
280001     .                                                                    
280101     SKIP2                                                                
280201                                                                          
280301 IMS-GU-UPFA01 SECTION.                                                   
280401     STRING 'W6UPFA01(IDLOPNRM =' W1-IDLOPNRM-X ')'                       
280501          DELIMITED BY SIZE INTO SSA1                                     
280601     MOVE '  GE' TO GODK-STATUSKODER                                      
280701     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA-UPFA01 SSA1               
280801     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
280901     PERFORM IMS-STATUSKONTROLL                                           
281001     .                                                                    
281101     SKIP3                                                                
281201                                                                          
281301 IMS-GNP-UPFA11 SECTION.                                                  
281401     STRING 'W6UPFA01(IDLOPNRM =' W1-IDLOPNRM-X ')'                       
281501          DELIMITED BY SIZE INTO SSA1                                     
281601     MOVE 'W6UPFA11 ' TO SSA2                                             
281701     MOVE '  GE' TO GODK-STATUSKODER                                      
281801     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
281901     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
282001     PERFORM IMS-STATUSKONTROLL                                           
282101     .                                                                    
282201     SKIP3                                                                
282301                                                                          
282401 IMS-GNP-UPFA12 SECTION.                                                  
282501     STRING 'W6UPFA01(IDLOPNRM =' W1-IDLOPNRM-X ')'                       
282601          DELIMITED BY SIZE INTO SSA1                                     
282701     MOVE 'W6UPFA12 ' TO SSA2                                             
282801     MOVE '  GE' TO GODK-STATUSKODER                                      
282901     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
283001     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
283101     PERFORM IMS-STATUSKONTROLL                                           
283201     .                                                                    
283301     SKIP3                                                                
283401                                                                          
283501 IMS-GU-WDB601    SECTION.                                                
283601     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
283701          DELIMITED BY SIZE INTO SSA1                                     
283801     MOVE '  GE' TO GODK-STATUSKODER                                      
283901     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
284001     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
284101     PERFORM IMS-STATUSKONTROLL                                           
284201     IF SEGMENT-SAKNAS                                                    
284301         MOVE SPACE TO DCS-KDDC                                           
284401     END-IF                                                               
284501     .                                                                    
284601                                                                          
284701 IMS-STATUSKONTROLL SECTION.                                              
284801                                                                          
284901     SET STATUS-IX TO 1                                                   
285001     SEARCH GODK-STATUS                                                   
285101       AT END                                                             
285201         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
285301         DELIMITED BY SIZE INTO FELTEXT                                   
285401         CALL FELLOG                                                      
285501       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
285601         CONTINUE                                                         
285701     END-SEARCH                                                           
286001     .                                                                    
