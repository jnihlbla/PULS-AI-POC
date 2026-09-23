000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4631300.                                                
000300 DATE-WRITTEN.   FEBR 1999.                                               
000400 AUTHOR          BO HAMMARIN, GDC-GROUP.                                  
000500     REMARKS.                                                             
000600*    FUNKTION.   SKRIVER PACKUNDERLAGSFIL TILL DIREKTLEVERANTÖR           
000700*                                                                         
000800*                FÖR KONRAD HÄMTAS NAMN OCH ADRESS FRÅN WDB7.             
000900*                                                                         
001000     SKIP2                                                                
001100 ENVIRONMENT DIVISION.                                                    
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400                                                                          
001410*    ---- XRST-FIL: KOPIA AV UTFIL(+0) TAS IN SOM INPUT-FIL               
001500         SELECT XRST-FIL     ASSIGN TO W46313D1.                          
001600                                                                          
001700     EJECT                                                                
001800 DATA DIVISION.                                                           
001900     SKIP2                                                                
002000 FILE SECTION.                                                            
002100                                                                          
002200 FD  XRST-FIL                                                             
002300     LABEL RECORD STANDARD                                                
002400     RECORDING V                                                          
002500     BLOCK CONTAINS 0.                                                    
002600                                                                          
002700 01  FILLER                  PIC X(998).                                  
002800 01  POST -COPY W46312       -PRE XRST-                                   
002900     EJECT                                                                
003000                                                                          
003100 WORKING-STORAGE SECTION.                                                 
003200     SKIP2                                                                
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  PROGRAM-NAMN            PIC X(8)  VALUE 'W4631300'.                  
003600 77  CURRENT-SECTION         PIC X(16) VALUE SPACE.                       
003700 77  CURRENT-IMS-SECTION     PIC X(16) VALUE SPACE.                       
003900 77  MSG-IO-AREA-LENGTH      PIC S9(9) VALUE +32 COMP SYNC.               
004000 77  MSG-IO-AREA             PIC X(32) VALUE SPACE.                       
004100 77  CHKP-AREA-1-LENGTH      PIC S9(9) VALUE +32 COMP SYNC.               
004200 77  CHKP-AREA-1             PIC X(32) VALUE SPACE.                       
004210 77  MAX-GSAM                PIC S9(4)   COMP.                            
004300 77  JA                      PIC X       VALUE 'J'.                       
004400 77  NEJ                     PIC X       VALUE 'N'.                       
004410 77  W-ANT-POSTER            PIC S9(7) COMP-3 VALUE ZERO.                 
004500 77  WS-CHKP-RAKNARE         PIC S9(3) COMP-3 VALUE ZERO.                 
004600 77  WS-CHKP-MAX             PIC S9(3) COMP-3 VALUE +10.                  
004601*77  WS-CHKP-MAX             PIC S9(3) COMP-3 VALUE +100.                 
004610 77  XRST-FIL-EOF            PIC X       VALUE 'N'.                       
004611                                                                          
004612 01  WS-TID                  PIC 9(8)  VALUE ZERO.                        
004613 01  FILLER REDEFINES WS-TID.                                             
004614     03  WS-TID-TTMMSS       PIC 9(6).                                    
004615     03  WS-TID-HH           PIC 9(2).                                    
004620 01  WS-FLD.                                                              
004640     03  WS-ANT-POST-GSAM    PIC S9(5)   COMP-3  VALUE ZERO.              
004650     03  WS-ANT-POST-XRST    PIC S9(5)   COMP-3  VALUE ZERO.              
004700     EJECT                                                                
004900 01  W-NAMN                  PIC X(4)  VALUE SPACE.                       
005000 01  W-KVBEART               PIC 9(6)  VALUE ZERO.                        
005010                                                                          
005020 01  W-DASKEPPN              PIC 9(8).                                    
005030 01  FILLER REDEFINES W-DASKEPPN.                                         
005040     03  FILLER              PIC 9(2).                                    
005050     03  W-TISKEPPN          PIC 9(6).                                    
005100                                                                          
005200                                                                          
005300 01  W-BEGMT.                                                             
005400     03  W-BEGMT-RAD1        PIC X(35) VALUE SPACE.                       
005500     03  W-BEGMT-RAD2        PIC X(35) VALUE SPACE.                       
005600 01  W-ADGMT.                                                             
005700     03  W-ADGMT-GATA        PIC X(35) VALUE SPACE.                       
005800     03  W-ADGMT-PADR        PIC X(35) VALUE SPACE.                       
005900     03  W-ADGMT-LAND        PIC X(35) VALUE SPACE.                       
006000                                                                          
006100 01  W-BEGODSM.                                                           
006200     03  W-BEGODSM-1         PIC X(27) VALUE SPACE.                       
006300     03  W-BEGODSM-2         PIC X(27) VALUE SPACE.                       
006400                                                                          
006500 01  W-ADGODSM.                                                           
006600     03  W-ADGODSM-1         PIC X(27) VALUE SPACE.                       
006700     03  W-ADGODSM-2         PIC X(27) VALUE SPACE.                       
006800                                                                          
006900 01 WS-POSTADRESS.                                                        
007000     03  WS-POSTNR           PIC X(10).                                   
007100     03  WS-ORT              PIC X(20).                                   
007200                                                                          
007300 01  DYNAMISK-SUBMODUL.                                                   
007400     03  FELLOG              PIC X(8) VALUE  'FELLOG  '.                  
007500     03  ABEND               PIC X(8) VALUE  'ABEND   '.                  
007600     03  CBLTDLI             PIC X(8) VALUE  'CBLTDLI '.                  
007700     03  POSTSUM             PIC X(8) VALUE  'POSTSUM '.                  
007800     EJECT                                                                
007900 01  TIDS-FALT.                                                           
008000     03  TID-TTMMSSHH         PIC 9(8).                                   
008100     03  FILLER               REDEFINES TID-TTMMSSHH.                     
008200         05  TID-TTMMSS       PIC 9(6).                                   
008300         05  TID-HH           PIC 9(2).                                   
008400                                                                          
008500 01  ABENDKODER.                                                          
008600    03  RKOD-ABEND-MED-DUMP  PIC S9(4) COMP SYNC VALUE +1000.             
008700    03  RKOD-ABEND-UTAN-DUMP PIC S9(4) COMP SYNC VALUE   +16.             
008800     EJECT                                                                
008900                                                                          
009300*01  FILLER -COPY W463CTRY.                                               
009400     EJECT                                                                
009500******************************************************************        
009600*    AREA FÖR POSTSUM                                            *        
009700******************************************************************        
009800                                                                          
009900*    -COPY W0005  -PRE POSTSUM-.                                          
010000     EJECT                                                                
010100                                                                          
010200******************************************************************        
010300*    AREA FÖR GENERELL DIREKTLEVERANSFIL                         *        
010400******************************************************************        
010500                                                                          
010600*01  UT-AREA  -COPY W46312.                                               
010700     EJECT                                                                
010800******************************************************************        
010900*    I M S  W S                                                  *        
011000******************************************************************        
011100                                                                          
011200 01   FILLER                 PIC X(16)   VALUE ALL 'I M S  W S'.          
011300 01  W-IDPRODNR-X.                                                        
011400   03  W-IDPRODNR            PIC S9(7)   COMP-3 VALUE ZERO.               
011500                                                                          
011510 01  W-DASNDDAT-X.                                                        
011520     03 W-DASNDDAT           PIC 9(8)  VALUE ZERO.                        
011521     03 FILLER REDEFINES W-DASNDDAT.                                      
011522        05  FILLER           PIC 9(2).                                    
011523        05  W-TISNDDAT       PIC 9(6).                                    
011530                                                                          
011600 01  W-IDGMT-X-B7.                                                        
011700   03  W-IDDISTR-B7          PIC S9(5)   VALUE ZERO COMP-3.               
011800   03  W-IDKUNDNR-B7         PIC S9(7)   VALUE ZERO COMP-3.               
011900                                                                          
011910 01  W-IDLEVNR-F4-X.                                                      
011920   03  W-IDLEVNR-F4          PIC X(5)    VALUE SPACE.                     
011940                                                                          
011950 01  W-WDGXKY-X.                                                          
011960     03  W-IDHTYP            PIC X(4)     VALUE '4579'.                   
011970     03  W-IDPGM             PIC X(8)     VALUE 'W4631300'.               
011971     03  W-LOW-VALUE         PIC X(18)    VALUE LOW-VALUE.                
011980                                                                          
011990 01  W-WDE4BSEQ-X.                                                        
011991     03  W-IDPRODNR-E4       PIC S9(7)    VALUE ZERO COMP-3.              
011992     03  W-IDPURAD-E4        PIC S9(5)    VALUE ZERO COMP-3.              
011993                                                                          
011995 01  W-IDGMTREF-X.                                                        
011996     03  W-IDDISTR-I2        PIC S9(5)    VALUE ZERO COMP-3.              
011997     03  W-IDKUNDNR-I2       PIC S9(7)    VALUE ZERO COMP-3.              
011998     03  W-IDORDNR-I2        PIC  9(7)    VALUE ZERO.                     
011999     03  FILLER              PIC  X(3)    VALUE SPACE.                    
012000                                                                          
012010 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS  OCCURS 5  INDEXED BY STATUS-IX                      
012200                             PIC X(2).                                    
012300                                                                          
012400 01  STATUS-WS               PIC X(2).                                    
012500     88  SEGMENT-FINNS                   VALUE '  '.                      
012600     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
012700     88  SEGMENT-SLUT                    VALUE 'GB'.                      
012800     88  IMS-EJ-OK                       VALUE 'XD'.                      
012900                                                                          
013000 01  ALL-SSA.                                                             
013100     03 SSA1                 PIC X(64).                                   
013110     03 SSA2                 PIC X(64).                                   
013200     EJECT                                                                
013300                                                                          
013400*    -COPY W0003.                                                         
013500     EJECT                                                                
013600******************************************************************        
013700*    DLI-IO-AREAOR                                               *        
013800******************************************************************        
013900                                                                          
014000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF601  '.                    
014100 01  DLI-IO-WDF601.                                                       
014200*    03  -COPY WDF601                                                     
014300     EJECT                                                                
014400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF611  '.                    
014500 01  DLI-IO-WDF611.                                                       
014600*    03  -COPY WDF611                                                     
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'WDB701-AREA'.         
014900 01  DLI-IO-AREA-B701.                                                    
015000*    03  -COPY WDB701                                                     
015010                                                                          
015020 01  FILLER               PIC X(16)   VALUE 'WDF401 AREA'.                
015030 01   DLI-IO-AREA-F401.                                                   
015040*     03  -COPY WDF401                                                    
015050                                                                          
015060 01  FILLER               PIC X(16)   VALUE 'WDF411 AREA'.                
015070 01   DLI-IO-AREA-F411.                                                   
015080*     03  -COPY WDF411                                                    
015100                                                                          
015101                                                                          
015102 01  FILLER                  PIC X(16)   VALUE 'WDE411   AREA'.           
015103 01  DLI-IO-WDE411.                                                       
015104*    03  FILLER  -COPY WDE411                                             
015105                                                                          
015106                                                                          
015107 01  FILLER                      PIC X(16)   VALUE 'WDI2-AREA'.           
015108 01   DLI-IO-WDI2.                                                        
015109      03  -COPY WDI201.                                                   
015110                                                                          
015111                                                                          
015112 01  FILLER                  PIC X(16)   VALUE 'WDGX4580 AREA'.           
015113 01  DLI-IO-WDGX4580.                                                     
015114*    03  FILLER  -COPY WDGX4580                                           
015115                                                                          
015116                                                                          
015117 01  FILLER                    PIC X(16) VALUE 'GSAMFIL-IO-AREA'.         
015118 01  GSAMFIL-IO-AREA.                                                     
015119     03  GSAM-LRECL            PIC S9(4) COMP.                            
015120*    03 -COPY W46312          -PRE  GSAM-                                 
015121                                                                          
015130                                                                          
015200 LINKAGE SECTION.                                                         
015300     SKIP2                                                                
015400*01  -COPY W0008      -PRE MSG-.                                          
015500         05    FILLER        PIC X.                                       
015600     EJECT                                                                
015700*01  -COPY W0008      -PRE WDF6-.                                         
015800         05    FILLER        PIC X.                                       
015900     EJECT                                                                
016000*01  -COPY W0008  -PRE WDB7-                                              
016100     05  FILLER                  PIC X.                                   
016110     EJECT                                                                
016120*01  -COPY W0008  -PRE WDF4-                                              
016130     05  FILLER                  PIC X.                                   
016131     EJECT                                                                
016132*01  -COPY W0008  -PRE WDR4-                                              
016133     05  FILLER                  PIC X.                                   
016134     EJECT                                                                
016135*01  -COPY W0008  -PRE WDE4-                                              
016136     05  FILLER                  PIC X.                                   
016137     EJECT                                                                
016138*01  -COPY W0008  -PRE WDI2-                                              
016139     05  FILLER                  PIC X.                                   
016140     EJECT                                                                
016141 01  -COPY W0008  -PRE  GSAMFIL-                                          
016150       05  FILLER                PIC X.                                   
016200     EJECT                                                                
016300 PROCEDURE DIVISION  USING MSG-PCB WDF6-PCB WDB7-PCB WDF4-PCB             
016310                                   WDR4-PCB WDE4-PCB WDI2-PCB             
016320                                   GSAMFIL-PCB.                           
016400     ENTRY 'DLITCBL' USING MSG-PCB WDF6-PCB WDB7-PCB WDF4-PCB             
016410                                   WDR4-PCB WDE4-PCB WDI2-PCB             
016420                                   GSAMFIL-PCB.                           
016500                                                                          
016600     PERFORM A-INIT                                                       
016700                                                                          
016800     PERFORM IMS-GN-WDF601                                                
017300                                                                          
017400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
017420*    if PUDH-IDPRODNR = 1032535                                           
017430*       CALL ABEND                                                        
017440*    END-IF                                                               
018300        PERFORM C-SKRIV-HUVUD                                             
018301        PERFORM D-SKAPA-WDF401                                            
018310                                                                          
019000        PERFORM IMS-GNP-WDF611                                            
019100                                                                          
019200        PERFORM UNTIL SEGMENT-SAKNAS                                      
019300           PERFORM E-SKRIV-RAD                                            
019400           PERFORM F-SKAPA-WDF411                                         
020000           PERFORM IMS-GNP-WDF611                                         
020100        END-PERFORM                                                       
020200                                                                          
020210        MOVE PUDH-IDPRODNR TO W-IDPRODNR                                  
020300        PERFORM IMS-GHU-WDF601                                            
020400        PERFORM IMS-DLET-WDF601                                           
020500        IF WS-CHKP-RAKNARE >= WS-CHKP-MAX                                 
020600           MOVE +0            TO WS-CHKP-RAKNARE                          
020700           PERFORM G-TAG-CHECKPOINT                                       
020710        END-IF                                                            
020720        PERFORM IMS-GN-WDF601                                             
020800     END-PERFORM                                                          
022200                                                                          
022300     PERFORM Z-FINIT                                                      
022400                                                                          
022500     MOVE ZERO                 TO RETURN-CODE                             
022600     GOBACK                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 A-INIT      SECTION.                                                     
022910     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
022920                                                                          
022930     MOVE +347               TO MAX-GSAM                                  
022940***** OBS  GSAM-LÄNGDEN SKALL VARA POSTLÄNGDEN PLUS 2 BYTES (VB)**        
023200                                                                          
023300     MOVE FUNCTION CURRENT-DATE (1:8) TO W-DASNDDAT                       
023301*    move 20190411 to w-dasnddat                                          
023310     ACCEPT WS-TID FROM TIME                                              
023311                                                                          
023312     PERFORM IMS-RESTART                                                  
023313     PERFORM IMS-LAS-ATERSTART                                            
023315     PERFORM IMS-GSAMFIL-OPEN                                             
023316                                                                          
023326     IF 4580-KVPOST > ZERO                                                
023327        PERFORM AA-INIT-BMP                                               
023328     END-IF                                                               
023329     .                                                                    
023330                                                                          
023331 AA-INIT-BMP SECTION.                                                     
023332                                                                          
023340     MOVE NEJ                TO XRST-FIL-EOF                              
023350     MOVE ZERO               TO WS-ANT-POST-XRST                          
023360                                WS-ANT-POST-GSAM                          
023380                                                                          
023394                                                                          
023396                                                                          
023397*--------------------------- XRST-FIL KOPIERAS TILL GSAMFILEN             
023398*                            FRAM TILL CHECKPOINT-LÄGE                    
023399     OPEN INPUT  XRST-FIL                                                 
023400                                                                          
023401     PERFORM AAA-LAS-XRST                                                 
023402                                                                          
023403     PERFORM UNTIL (XRST-FIL-EOF = JA                                     
023404                OR  WS-ANT-POST-XRST >= 4580-KVPOST)                      
023405        MOVE XRST-POST   TO GSAM-PU-W46312                                
023407        PERFORM S01-SKRIV-GSAM                                            
023408        PERFORM AAA-LAS-XRST                                              
023409     END-PERFORM                                                          
023410                                                                          
023411     IF  XRST-FIL-EOF      = JA                                           
023412     OR  WS-ANT-POST-XRST  NOT = 4580-KVPOST                              
023419         DISPLAY 'W4631300: FEL I ÅTERSTARTEN, RESTART-FIL'               
023420         CALL ABEND USING    RKOD-ABEND-UTAN-DUMP                         
023421     END-IF                                                               
023422                                                                          
023423*------- HÄR KOPIERAS DEN XRST-POST TILL GSAM-FILEN,                      
023424*        SOM VID FÖREGÅENDE EXEKVERING, VAR DEN SISTA SOM                 
023425*        SKREVS PÅ GSAM-FILEN FÖRE SISTA CHKP.                            
023426                                                                          
023427     MOVE XRST-POST      TO GSAM-PU-W46312                                
023429     PERFORM S01-SKRIV-GSAM                                               
023430                                                                          
023431     CLOSE XRST-FIL                                                       
023463     .                                                                    
023464     EJECT                                                                
023465 AAA-LAS-XRST SECTION.                                                    
023466                                                                          
023467     READ XRST-FIL                                                        
023468          AT END MOVE JA TO XRST-FIL-EOF                                  
023469     END-READ                                                             
023470     IF XRST-FIL-EOF = NEJ                                                
023471        MOVE 'W46313'    TO POSTSUM-FDNAMN                                
023472        MOVE 'W46313D1'  TO POSTSUM-DDNAMN2                               
023473        MOVE 'XRST'      TO POSTSUM-TRANSTYP                              
023474        CALL POSTSUM USING POSTSUM-PARM                                   
023475        ADD +1           TO WS-ANT-POST-XRST                              
023476     END-IF                                                               
023480     .                                                                    
023500     EJECT                                                                
024600 C-SKRIV-HUVUD      SECTION.                                              
024610     MOVE 'C-SKRIV-HUVUD   ' TO CURRENT-SECTION                           
024700                                                                          
024800     MOVE SPACE               TO PU-W46312                                
024900     MOVE 'HUV'               TO PU-IDPTYP                                
025000     MOVE PUDH-IDLEVNR        TO PU-IDLEVNR                               
025100     MOVE PUDH-IDPRODNR       TO PU-IDPRODNR                              
025200     MOVE PUDH-IDDISTR        TO PU-IDDISTR                               
025300     MOVE PUDH-IDKUNDNR       TO PU-IDKUNDNR                              
025400     MOVE PUDH-IDORDNR7       TO PU-IDORDNR                               
025500     MOVE PUDH-IDDC           TO PU-IDDC                                  
025600     MOVE PUDH-IDDEPOT        TO PU-IDDEPOT                               
025700     MOVE PUDH-IDROUTE        TO PU-IDROUTE                               
025800     MOVE PUDH-IDVAT          TO PU-IDVAT                                 
025900     MOVE PUDH-IDZON          TO PU-IDZON                                 
026000     MOVE PUDH-ADGMT          TO PU-ADGMT                                 
026100     MOVE PUDH-BEGMRK         TO PU-BEGDSMRK                              
026200     MOVE PUDH-BEGMT          TO PU-BEGMT                                 
026300     MOVE PUDH-BELAGINS-DIR   TO PU-BELAGINS                              
026400     MOVE W-DASNDDAT          TO PU-DAUTSKR                               
026500     MOVE PUDH-DASKEPPN       TO PU-DASKEPPN                              
026600     MOVE PUDH-DASNDDAT       TO PU-DASNDDAT                              
026700     MOVE WS-TID              TO PU-TISNDTID                              
026800     MOVE PUDH-KDFRAKT        TO PU-KDFRAKT                               
026900     MOVE PUDH-KDORDKL        TO PU-KDORDKL                               
027000     MOVE PUDH-KDVIA          TO PU-KDVIA                                 
027100                                                                          
027200     IF PUDH-IDLEVNR = '6492 '                                            
027300     OR PUDH-IDLEVNR = 'BZFFA'                                            
027400       MOVE PUDH-IDDISTR      TO W-IDDISTR-B7                             
027500       MOVE PUDH-IDKUNDNR     TO W-IDKUNDNR-B7                            
027600                                                                          
027700       PERFORM IMS-GU-WDB701                                              
027800                                                                          
027900       IF SEGMENT-FINNS                                                   
028000       AND GMTD-IDLANDX2 NOT = 'JP'                                       
028100         MOVE GMTD-BEDEALER-VIPSGMT TO PU-BEGMT-RAD1                      
028200         MOVE GMTD-ADDEALER-GMTRAD1 TO PU-BEGMT-RAD2                      
028300         MOVE GMTD-ADDEALER-GMTRAD2 TO PU-ADGMT-GATA                      
028400         MOVE GMTD-ADPOSTNR-GMT     TO WS-POSTNR                          
028500*---------------FIX FÖR ATT TA BORT BLANK I SVENSKA POSTNUMMER            
028600         IF  GMTD-IDLANDX2 = 'SE'                                         
028700         AND WS-POSTNR(1:3) NUMERIC                                       
028800         AND WS-POSTNR(4:1) = ' '                                         
028900         AND WS-POSTNR(5:2) NUMERIC                                       
029000           MOVE WS-POSTNR(5:6)      TO WS-POSTNR(4:7)                     
029100         END-IF                                                           
029200*--------------END FIX (BO S)                                             
029300*---------------FIX FÖR ATT TA BORT DK- I DANSKA  POSTNUMMER              
029400         IF  GMTD-IDLANDX2 = 'DK'                                         
029500         AND WS-POSTNR(1:3) = 'DK-'                                       
029600           MOVE WS-POSTNR(4:7)      TO WS-POSTNR(1:10)                    
029700         END-IF                                                           
029800*--------------END FIX (BO S)                                             
029900         MOVE GMTD-ADCITY-GMT       TO WS-ORT                             
030000         MOVE WS-POSTADRESS         TO PU-ADGMT-PADR                      
030100         IF  GMTD-IDLANDX2 = 'AT'                                         
030200           SEARCH ALL AT-LAND-ING                                         
030300              AT END                                                      
030400                 CONTINUE                                                 
030500              WHEN AT-SOK(AT-IX) = PUDH-IDDISTR                           
030600                 MOVE AT-COUNTRY(AT-IX)                                   
030700                              TO PU-ADGMT-LAND                            
030800           END-SEARCH                                                     
030900         ELSE                                                             
031000           SEARCH ALL X2-LAND-ING                                         
031100              AT END                                                      
031200                 CONTINUE                                                 
031300              WHEN X2-SOK(X2-IX) = GMTD-IDLANDX2                          
031400                 MOVE X2-COUNTRY(X2-IX)                                   
031500                              TO PU-ADGMT-LAND                            
031600           END-SEARCH                                                     
031700         END-IF                                                           
031800       END-IF                                                             
031900     END-IF                                                               
031910*---------------FIX FÖR ATT justera begmt till edi                        
031911     IF  PU-BEGMT-RAD1 = SPACE                                            
031912     AND PU-BEGMT-RAD2 NOT = SPACE                                        
031913         MOVE PU-BEGMT-RAD2 TO PU-BEGMT-RAD1                              
031914         MOVE SPACE         TO PU-BEGMT-RAD2                              
031915     END-IF                                                               
031920*---------------END FIX (gk)                                              
032000                                                                          
032100     MOVE UT-AREA             TO GSAM-PU-W46312                           
032110     PERFORM S01-SKRIV-GSAM                                               
032400     .                                                                    
032500     EJECT                                                                
032510 D-SKAPA-WDF401    SECTION.                                               
032520     MOVE 'D-SKAPA-WDF401  ' TO CURRENT-SECTION                           
032530                                                                          
032531     MOVE PUDH-IDLEVNR       TO DLEV-IDLEVNR                              
032532                                W-IDLEVNR-F4                              
032533     PERFORM IMS-ISRT-WDF401                                              
032534     .                                                                    
032540     EJECT                                                                
032600 E-SKRIV-RAD              SECTION.                                        
032610     MOVE 'E-SKRIV-RAD     ' TO CURRENT-SECTION                           
032700                                                                          
032800     MOVE SPACE               TO PU-W46312                                
032900     MOVE 'RAD'               TO PU-IDPTYP                                
033000     MOVE PUDH-IDLEVNR        TO PU-IDLEVNR                               
033100     MOVE PUDH-IDPRODNR       TO PU-IDPRODNR                              
033200     MOVE PUDR-IDPURAD        TO PU-IDRADNR                               
033300     MOVE PUDR-IDARTNR        TO PU-IDARTNR                               
033400     MOVE PUDR-ADART          TO PU-ADART                                 
033500     MOVE PUDR-BEART          TO PU-BEART                                 
033600*** FUL FIX FÖR VARTA SOM INTE KLARAR BLANKT MEDDELANDE, TL 041103        
033700     IF (PUDH-IDLEVNR = 'S5PQB' AND PUDR-BERADREF = SPACE)                
033800     OR (PUDH-IDLEVNR = 'V022A' AND PUDR-BERADREF = SPACE)                
033900        MOVE 'XXX' TO PUDR-BERADREF                                       
034000     END-IF                                                               
034100                                                                          
034200     MOVE PUDR-BERADREF       TO PU-BERADREF                              
034300     MOVE PUDR-KDARTURS       TO PU-KDARTURS                              
034400     MOVE PUDR-KVBEART        TO PU-KVBEART                               
034500     MOVE PUDR-PRARTNTO       TO PU-PRARTNTO                              
034501     MOVE PUDH-TIREPDAT       TO PU-TIREPDAT                              
034502                                                                          
034503     MOVE PUDH-IDDISTR        TO W-IDDISTR-I2                             
034504     MOVE PUDH-IDKUNDNR       TO W-IDKUNDNR-I2                            
034505     MOVE PUDH-IDORDNR7       TO W-IDORDNR-I2                             
034507     PERFORM IMS-GU-WDI201                                                
034508     IF SEGMENT-FINNS                                                     
034509        MOVE TAKF-IDBILREG    TO PU-IDBILREG                              
034510        MOVE TAKF-BEMEKAN     TO PU-BEMEKAN                               
034511     END-IF                                                               
034512                                                                          
034513     MOVE PUDH-IDPRODNR       TO W-IDPRODNR-E4                            
034514     MOVE PUDR-IDPURAD        TO W-IDPURAD-E4                             
034515     PERFORM IMS-GU-WDE411-BSEQ                                           
034516     IF SEGMENT-FINNS                                                     
034520        MOVE ORAD-IDKUNDRF-WIP   TO PU-IDKUNDRF-WIP                       
034530     END-IF                                                               
034600                                                                          
034700     MOVE UT-AREA             TO GSAM-PU-W46312                           
034710     PERFORM S01-SKRIV-GSAM                                               
035000     .                                                                    
035100     EJECT                                                                
035200 F-SKAPA-WDF411    SECTION.                                               
035300     MOVE 'F-SKAPA-WDF411  ' TO CURRENT-SECTION                           
035310                                                                          
035311     MOVE PUDH-IDPRODNR      TO DLOR-IDPRODNR                             
035314     MOVE PUDR-IDPURAD       TO DLOR-IDPURAD                              
035317     MOVE W-TISNDDAT         TO DLOR-TIUTSKR                              
035320     MOVE PUDR-IDARTNR       TO DLOR-IDARTNR                              
035323     MOVE PUDH-IDDISTR       TO DLOR-IDDISTR                              
035326     MOVE PUDH-IDKUNDNR      TO DLOR-IDKUNDNR                             
035329     MOVE PUDH-IDORDNR7      TO DLOR-IDORDNR7                             
035332     MOVE ZERO               TO DLOR-KDANNULL                             
035335     MOVE ZERO               TO DLOR-KVSLULEV                             
035338     MOVE ZERO               TO DLOR-TIANNULL                             
035341     MOVE ZERO               TO DLOR-TISLULEV                             
035342     MOVE PUDH-DASKEPPN      TO W-DASKEPPN                                
035344     MOVE W-TISKEPPN         TO DLOR-TISKEPPN                             
035345     MOVE WS-TID-TTMMSS      TO DLOR-TIUTSTID                             
035346     MOVE ZERO               TO DLOR-TIPACKN                              
035347                                                                          
035348     PERFORM IMS-ISRT-WDF411                                              
035349     .                                                                    
035350     EJECT                                                                
035351 G-TAG-CHECKPOINT SECTION.                                                
035352                                                                          
035353*    UPPDATERA ÅTERSTARTREGISTRET                                         
035354     PERFORM IMS-LAS-ATERSTART                                            
035355     MOVE WS-ANT-POST-GSAM TO 4580-KVPOST                                 
035356     ACCEPT 4580-TIUPPDAT FROM DATE                                       
035357     ACCEPT 4580-TIUPPTID FROM TIME                                       
035358                                                                          
035359     PERFORM IMS-REPL-ATERSTART                                           
035360                                                                          
035361*    TAG CHECKPOINT                                                       
035362     PERFORM IMS-CHECKPOINT                                               
035363     .                                                                    
035364     EJECT                                                                
035365 Z-FINIT           SECTION.                                               
035366     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
035367                                                                          
035368*    NOLLA ÅTERSTARTINFORMATIONEN                                         
035369     PERFORM IMS-LAS-ATERSTART                                            
035370     MOVE +0                   TO 4580-KVPOST                             
035371     ACCEPT 4580-TIUPPDAT FROM DATE                                       
035372     ACCEPT 4580-TIUPPTID FROM TIME                                       
035373     PERFORM IMS-REPL-ATERSTART                                           
035380                                                                          
035390     PERFORM IMS-GSAMFIL-CLOSE                                            
035391                                                                          
035400     MOVE 'S'             TO   POSTSUM-OPKOD                              
035500     CALL POSTSUM USING POSTSUM-PARM                                      
035700     .                                                                    
035800     EJECT                                                                
037810 S01-SKRIV-GSAM SECTION.                                                  
037820                                                                          
037821     MOVE MAX-GSAM       TO GSAM-LRECL                                    
037830     PERFORM IMS-GSAMFIL-ISRT                                             
037840     ADD +1              TO WS-ANT-POST-GSAM                              
037850     .                                                                    
037900     EJECT                                                                
038000                                                                          
038100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
038200*                                                           *             
038300*   I M S - S E K T I O N E R                               *             
038400*                                                           *             
038500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
038600                                                                          
038700 IMS-GN-WDF601 SECTION.                                                   
038710     MOVE 'IMS-GN-WDF601   ' TO CURRENT-IMS-SECTION                       
038800                                                                          
038810     MOVE SPACE              TO ALL-SSA                                   
038900     STRING 'WDF601  (DASNDDAT<=' W-DASNDDAT-X ')'                        
039000             DELIMITED BY SIZE INTO SSA1                                  
039100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
039200     CALL CBLTDLI USING GN WDF6-PCB DLI-IO-WDF601 SSA1                    
039300     MOVE WDF6-STATUS-CODE TO STATUS-WS                                   
039500     PERFORM IMS-STATUSKONTROLL                                           
039600     .                                                                    
039700     SKIP2                                                                
039710 IMS-GHU-WDF601 SECTION.                                                  
039720     MOVE 'IMS-GHU-WDF601  ' TO CURRENT-IMS-SECTION                       
039730                                                                          
039731     MOVE SPACE              TO ALL-SSA                                   
039740     STRING 'WDF601  (IDPRODNR =' W-IDPRODNR-X ')'                        
039750             DELIMITED BY SIZE INTO SSA1                                  
039760     MOVE '  ' TO GODK-STATUSKODER                                        
039770     CALL CBLTDLI USING GHU WDF6-PCB DLI-IO-WDF601 SSA1                   
039780     MOVE WDF6-STATUS-CODE TO STATUS-WS                                   
039791     PERFORM IMS-STATUSKONTROLL                                           
039792     .                                                                    
039793     SKIP2                                                                
039800 IMS-GNP-WDF611 SECTION.                                                  
039810     MOVE 'IMS-GNP-WDF611  ' TO CURRENT-IMS-SECTION                       
039900                                                                          
039910     MOVE SPACE            TO ALL-SSA                                     
040000     MOVE 'WDF611 '        TO SSA1                                        
040200     MOVE '  GEGB'         TO GODK-STATUSKODER                            
040300     CALL CBLTDLI USING GNP WDF6-PCB DLI-IO-WDF611 SSA1                   
040400     MOVE WDF6-STATUS-CODE TO STATUS-WS                                   
040600     PERFORM IMS-STATUSKONTROLL                                           
040700     .                                                                    
040800     SKIP2                                                                
040900 IMS-DLET-WDF601 SECTION.                                                 
040910     MOVE 'IMS-DLET-WDF601 ' TO CURRENT-IMS-SECTION                       
041000                                                                          
041010     MOVE SPACE            TO ALL-SSA                                     
041100     MOVE '  ' TO GODK-STATUSKODER                                        
041200     CALL CBLTDLI USING DLET WDF6-PCB DLI-IO-WDF601                       
041300     MOVE WDF6-STATUS-CODE TO STATUS-WS                                   
041500     PERFORM IMS-STATUSKONTROLL                                           
041510                                                                          
041520     ADD +2                   TO WS-CHKP-RAKNARE                          
041600     .                                                                    
041700                                                                          
045800 IMS-GU-WDB701 SECTION.                                                   
045900                                                                          
045910     MOVE SPACE            TO ALL-SSA                                     
046000     STRING 'WDB701  (IDGMT    =' W-IDGMT-X-B7 ')'                        
046100          DELIMITED BY SIZE INTO SSA1                                     
046200     MOVE '  GE' TO GODK-STATUSKODER                                      
046300     CALL CBLTDLI USING GU WDB7-PCB DLI-IO-AREA-B701 SSA1                 
046400     MOVE WDB7-STATUS-CODE TO STATUS-WS                                   
046500     PERFORM IMS-STATUSKONTROLL                                           
046600     .                                                                    
046700                                                                          
046710 IMS-ISRT-WDF401      SECTION.                                            
046720     MOVE 'IMS-ISRT-WDEF01 ' TO CURRENT-IMS-SECTION                       
046730                                                                          
046740     MOVE SPACE              TO ALL-SSA                                   
046750     MOVE 'WDF401 '          TO SSA1                                      
046760     MOVE '  II'             TO GODK-STATUSKODER                          
046770     CALL CBLTDLI USING ISRT WDF4-PCB DLI-IO-AREA-F401 SSA1               
046780     MOVE WDF4-STATUS-CODE   TO STATUS-WS                                 
046790     PERFORM IMS-STATUSKONTROLL                                           
046791                                                                          
046792     ADD +1                   TO WS-CHKP-RAKNARE                          
046793     .                                                                    
046794                                                                          
046795                                                                          
046796 IMS-ISRT-WDF411      SECTION.                                            
046797     MOVE 'IMS-ISRT-WDF411 ' TO CURRENT-IMS-SECTION                       
046798                                                                          
046799     MOVE SPACE              TO ALL-SSA                                   
046800     STRING 'WDF401  (IDLEVNR  =' W-IDLEVNR-F4-X ')'                      
046801         DELIMITED BY SIZE INTO SSA1                                      
046802     MOVE 'WDF411 '          TO SSA2                                      
046803     MOVE '  '               TO GODK-STATUSKODER                          
046804     CALL CBLTDLI USING ISRT WDF4-PCB DLI-IO-AREA-F411 SSA1 SSA2          
046805     MOVE WDF4-STATUS-CODE   TO STATUS-WS                                 
046806     PERFORM IMS-STATUSKONTROLL                                           
046807                                                                          
046808     ADD +1                   TO WS-CHKP-RAKNARE                          
046809     .                                                                    
046810                                                                          
046811                                                                          
046812 IMS-GU-WDE411-BSEQ SECTION.                                              
046813     MOVE 'IMS-GU-WDE411-B'  TO CURRENT-IMS-SECTION                       
046814                                                                          
046815     STRING 'WDE411  (WDE4BSEQ =' W-WDE4BSEQ-X ')'                        
046816          DELIMITED BY SIZE INTO SSA1                                     
046817     MOVE '  GE' TO GODK-STATUSKODER                                      
046818     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE411 SSA1                    
046819     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
046820     PERFORM IMS-STATUSKONTROLL                                           
046821     .                                                                    
046822                                                                          
046823                                                                          
046824 IMS-GU-WDI201      SECTION.                                              
046825     MOVE 'IMS-GU-WDI201  '  TO CURRENT-IMS-SECTION                       
046826                                                                          
046827     STRING 'WDI201  (IDGMTREF =' W-IDGMTREF-X ')'                        
046828            DELIMITED BY SIZE INTO SSA1                                   
046829     MOVE '  GE' TO GODK-STATUSKODER                                      
046830     CALL CBLTDLI USING GU WDI2-PCB DLI-IO-WDI2 SSA1                      
046831     MOVE WDI2-STATUS-CODE TO STATUS-WS                                   
046832     PERFORM IMS-STATUSKONTROLL                                           
046833     .                                                                    
046834                                                                          
046835                                                                          
046836 IMS-GSAMFIL-OPEN SECTION.                                                
046837                                                                          
046838     MOVE 'OUT' TO GSAMFIL-IO-AREA                                        
046839     MOVE '  ' TO GODK-STATUSKODER                                        
046840     CALL CBLTDLI USING OPEN-GSAM GSAMFIL-PCB GSAMFIL-IO-AREA             
046841     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
046842     PERFORM IMS-STATUSKONTROLL                                           
046843     .                                                                    
046844     SKIP2                                                                
046845 IMS-GSAMFIL-ISRT SECTION.                                                
046846                                                                          
046847     MOVE '  ' TO GODK-STATUSKODER                                        
046848     CALL CBLTDLI USING ISRT GSAMFIL-PCB GSAMFIL-IO-AREA                  
046849     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
046850     PERFORM IMS-STATUSKONTROLL                                           
046851     .                                                                    
046852     SKIP2                                                                
046853 IMS-GSAMFIL-CLOSE SECTION.                                               
046854                                                                          
046855     MOVE '  ' TO GODK-STATUSKODER                                        
046856     CALL CBLTDLI USING CLSE-GSAM GSAMFIL-PCB                             
046857     MOVE GSAMFIL-STATUS-CODE TO STATUS-WS                                
046858     PERFORM IMS-STATUSKONTROLL                                           
046859     .                                                                    
046860     EJECT                                                                
046870 IMS-RESTART  SECTION.                                                    
046900                                                                          
047000     MOVE SPACE TO MSG-IO-AREA                                            
047100     MOVE '  '  TO GODK-STATUSKODER                                       
047200     CALL CBLTDLI USING XRST MSG-PCB                                      
047300                          MSG-IO-AREA-LENGTH MSG-IO-AREA                  
047400                          CHKP-AREA-1-LENGTH CHKP-AREA-1                  
047500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
047600     PERFORM IMS-STATUSKONTROLL                                           
047700                                                                          
047800     IF IMS-EJ-OK                                                         
047900       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
048000       CALL FELLOG                                                        
048100     END-IF                                                               
048200     .                                                                    
048300     SKIP2                                                                
048400                                                                          
048410 IMS-LAS-ATERSTART SECTION.                                               
048420                                                                          
048460     STRING 'WDR401  (WDGXKEY  =' W-WDGXKY-X ')'                          
048470                    DELIMITED BY SIZE INTO SSA1                           
048480     MOVE 'WDR470   '    TO SSA2                                          
048490     MOVE '    '           TO GODK-STATUSKODER                            
048491     CALL CBLTDLI USING GHU WDR4-PCB DLI-IO-WDGX4580 SSA1 SSA2            
048492     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
048493     PERFORM IMS-STATUSKONTROLL                                           
048494     .                                                                    
048495                                                                          
048510 IMS-REPL-ATERSTART SECTION.                                              
048511     SKIP2                                                                
048512     MOVE '  '             TO GODK-STATUSKODER                            
048513     CALL CBLTDLI USING REPL WDR4-PCB DLI-IO-WDGX4580                     
048514     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
048515     PERFORM IMS-STATUSKONTROLL                                           
048516     .                                                                    
048517                                                                          
048520 IMS-CHECKPOINT SECTION.                                                  
048600                                                                          
048700     MOVE PROGRAM-NAMN TO MSG-IO-AREA                                     
048800     MOVE '  XD'       TO GODK-STATUSKODER                                
048900     CALL CBLTDLI USING CHKP MSG-PCB                                      
049000                          MSG-IO-AREA-LENGTH MSG-IO-AREA                  
049100                          CHKP-AREA-1-LENGTH CHKP-AREA-1                  
049200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049300     PERFORM IMS-STATUSKONTROLL                                           
049400     IF IMS-EJ-OK                                                         
049500       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
049600       CALL FELLOG                                                        
049700     END-IF                                                               
049800     .                                                                    
049900     EJECT                                                                
050000                                                                          
050100 IMS-STATUSKONTROLL SECTION.                                              
050200                                                                          
050300     SKIP2                                                                
050400     SET STATUS-IX TO 1                                                   
050500     SEARCH GODK-STATUS AT END CALL FELLOG                                
050600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
050700     CONTINUE                                                             
050800     .                                                                    
