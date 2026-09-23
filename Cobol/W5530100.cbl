000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5530100.                                                
000300 AUTHOR.         EGHOLT CONNY.                                            
000400 DATE-WRITTEN.   05/11/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET FIXAR RÄTT LAYOUT OCH KOLLAR INDATA                   
001000*        FÖR W553D2-RTN PÅ CLASSIC                                        
001100*        PARTS PRISUPPDATERINGAR.                                         
001200*                                                                         
001300*                                                                         
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0007 -  Programmet sätter denna för att rapportera fel          
001601*                 till user via mail. (WMEMOSND)                          
001610*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- PRISUPPDATERING FRÅN CLASSIC PARTS                         
002800     SELECT GCPFIL                     ASSIGN TO W55301D1.                
002900     SKIP2                                                                
003000*          --- MID-POSTER                                                 
003100     SELECT W55301                     ASSIGN TO W55301D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  GCPFIL                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000 01  CSV-RECORD            PIC X(67).                                     
004100     SKIP3                                                                
004200                                                                          
004300 FD  W55301                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600*01  POST -COPY W55310 -PRE  W55301-  -L.                                 
004700                                                                          
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100 77  IDPGM                       PIC X(8)    VALUE 'W5530100'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005310 77  DOTS                        PIC X       VALUE '.'.                   
005320 77  COLONS                      PIC X       VALUE ':'.                   
005330 77  COMMAS                      PIC X       VALUE ','.                   
005340 77  DOUBLE-SPACE                PIC XX      VALUE '  '.                  
005400 77  POS                         PIC S9(4)   VALUE ZERO COMP-3.           
005401 77  INTEGERS                    PIC S9(4)   VALUE ZERO COMP-3.           
005402 77  IND                         PIC S9(4)   VALUE ZERO COMP-3.           
005410 77  INDATA-RAKN                 PIC S9(4)   VALUE ZERO COMP-3.           
005411                                                                          
005420 77  WS-IDARTNR-NUM              PIC 9(8)    VALUE ZERO.                  
005500 77  WS-IDARTNR-X                PIC X(8)    VALUE SPACE.                 
005501                                                                          
005510 77  WS-PRARTBEL-NUM             PIC S9(7)V9(2) VALUE ZERO.               
005600                                                                          
005700 77  GCPFIL-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-GCPFIL                       VALUE 'J'.                   
005810     88  NOT-END-OF-GCPFIL                   VALUE 'N'.                   
005900                                                                          
006000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006100     88  INDATA-OK                           VALUE 'J'.                   
006200     88  INDATA-FEL                          VALUE 'N'.                   
006300                                                                          
006400     EJECT                                                                
006500 01  DAGENS-DATUM                PIC X(8)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(4).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007410     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007420     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007421     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007430     EJECT                                                                
007431*01 -COPY WDECAREA                                                        
007440     EJECT                                                                
007500     SKIP2                                                                
007510*01 -COPY WDATAREA                                                        
007520     EJECT                                                                
007530     SKIP2                                                                
007600*    --- PARAMETRAR TILL ABEND                                            
007700                                                                          
007800 77  RKOD-ABEND                  PIC S9(4) BINARY VALUE +0.               
007900 77  RKOD-SKAPA-FELMAIL          PIC S9(4) BINARY VALUE +7.               
007910 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4) BINARY VALUE +16.              
008000 77  RKOD-ABEND-MED-DUMP         PIC S9(4) BINARY VALUE +1000.            
008100     SKIP2                                                                
008200 01  FELTEXT.                                                             
008300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008400     03  FELTEXT-STR             PIC X(100)  VALUE SPACE.                 
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL POSTSUM                                          
008700*                                                                         
008800*01  -COPY W0005   -PRE  POSTSUM-                                         
008900     EJECT                                                                
009000 01  IN-AREA-START               PIC X(24)   VALUE                        
009100                                 'IN-AREA-START  '.                       
009200 01  IN-AREA                     PIC X(255).                              
009300                                                                          
009400 01  IN-GCP-AREA.                                                         
009500     03 IN-GCP-IDARTNR           PIC 9(9).                                
009510     03 IN-GCP-ANM               PIC X(15).                               
009520     03 IN-GCP-NYTT-GCPPRIS      PIC X(15).                               
009530     03 IN-GCP-RAB-KOD           PIC X(15).                               
009700     03 IN-GCP-PRARTBEL          PIC X(15).                               
009900     03 IN-GCP-TIPRLIST          PIC 9(8).                                
010000                                                                          
010100 01  TEST-AREA.                                                           
010300     03 TEST-IDARTNR             PIC X(15).                               
010310     03 TEST-ANM                 PIC X(15).                               
010320     03 TEST-NYTT-GCPPRIS        PIC X(15).                               
010330     03 TEST-RAB-KOD             PIC X(15).                               
010500     03 TEST-PRARTBEL            PIC X(15).                               
010600     03 TEST-TIPRLIST            PIC X(15).                               
010700                                                                          
010800     EJECT                                                                
010900 01  MID-AREA-START              PIC X(24)   VALUE                        
011000                                 'W55301-AREA-START  '.                   
011100*01  AREA -COPY W55310     -PRE W55301-                                   
011200                                                                          
011300     EJECT                                                                
011400 PROCEDURE DIVISION.                                                      
011500 MAIN SECTION.                                                            
011600     SKIP2                                                                
011700     PERFORM A-INIT                                                       
011800     PERFORM S01-LAES-GCPFIL                                              
012000     PERFORM UNTIL END-OF-GCPFIL                                          
012100                OR INDATA-FEL                                             
012110         IF INDATA-RAKN = +1                                              
012120*          --- Hoppa över första posten (RUBRIKERNA)                      
012121           PERFORM S01-LAES-GCPFIL                                        
012140         ELSE                                                             
012200           PERFORM B-KOLLA-INDATA                                         
012300           IF INDATA-OK                                                   
013100             PERFORM C-SKAPA-W55301                                       
013101             PERFORM S11-SKRIV-W55301                                     
013102             PERFORM S01-LAES-GCPFIL                                      
013103           END-IF                                                         
013105         END-IF                                                           
013600     END-PERFORM                                                          
013601                                                                          
013602                                                                          
013603     MOVE 'S' TO POSTSUM-OPKOD                                            
013604     CALL POSTSUM USING POSTSUM-PARM                                      
013702                                                                          
013710     IF INDATA-FEL                                                        
013720         DISPLAY ' '                                                      
013723         DISPLAY IN-AREA                                                  
013724         PERFORM S98-STOPPA-FIL-CLOSE-OPEN                                
013725         MOVE RKOD-SKAPA-FELMAIL TO RETURN-CODE                           
013730     ELSE                                                                 
013731         MOVE ZERO TO RETURN-CODE                                         
013810     END-IF                                                               
013812                                                                          
013813     PERFORM Z-FINIT                                                      
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014300     OPEN INPUT  GCPFIL                                                   
014400     OPEN OUTPUT W55301                                                   
014500     SKIP2                                                                
014610     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
014620                                                                          
014700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014701                                                                          
014702     MOVE ZERO TO INDATA-RAKN                                             
014703                                                                          
014704     INITIALIZE W55301-AREA                                               
014705     DISPLAY ' '                                                          
014800     .                                                                    
014900     EJECT                                                                
015000 B-KOLLA-INDATA  SECTION.                                                 
015100     SKIP2                                                                
015200     MOVE JA   TO INDATA-SW                                               
015210     MOVE +1   TO IND                                                     
015300     UNSTRING IN-AREA DELIMITED BY ';' OR '  ' INTO                       
015500                                  IN-GCP-IDARTNR                          
015501                                  IN-GCP-ANM                              
015502                                  IN-GCP-NYTT-GCPPRIS                     
015503                                  IN-GCP-RAB-KOD                          
015510                                  IN-GCP-PRARTBEL                         
015530                                  IN-GCP-TIPRLIST                         
015540                                                                          
015600     UNSTRING IN-AREA DELIMITED BY ';' OR '  ' INTO                       
015800                                  TEST-IDARTNR                            
015900                                  TEST-ANM                                
015910                                  TEST-NYTT-GCPPRIS                       
015920                                  TEST-RAB-KOD                            
016000                                  TEST-PRARTBEL                           
016100                                  TEST-TIPRLIST                           
016510                                                                          
016600*    --- ARTIKELNUMRET                                                    
016601     MOVE ZERO TO POS                                                     
016602     INSPECT TEST-IDARTNR TALLYING POS FOR CHARACTERS BEFORE              
016603                                            INITIAL '  '                  
016604     IF POS > +9                                                          
016605       MOVE NEJ TO INDATA-SW                                              
016607       MOVE 'IN-IDARTNR > 9 TKN :' TO FELTEXT-STR(IND:20)                 
016608       ADD +20 TO IND                                                     
016609       DISPLAY 'Partno. longer than 9 characters ' TEST-IDARTNR           
016620     ELSE                                                                 
016700       IF TEST-IDARTNR(1:POS) NOT NUMERIC                                 
016800         MOVE NEJ TO INDATA-SW                                            
016810         MOVE 'IN-IDARTNR NOT NUM :' TO FELTEXT-STR(IND:20)               
016820         ADD +20 TO IND                                                   
016830         DISPLAY 'Partno.is not numeric ' TEST-IDARTNR                    
016840       ELSE                                                               
016850         CONTINUE                                                         
016901       END-IF                                                             
016910     END-IF                                                               
017000                                                                          
017100*    --- BESTÄLLNINGSPRISET                                               
017101*    --- Kollar att priset har rätt format. Först ersätts alla            
017102*    --- varianter av decimaltecken för att minimera felorsaker.          
017103                                                                          
017104     MOVE ZERO TO POS                                                     
017105     INSPECT TEST-PRARTBEL TALLYING POS FOR CHARACTERS                    
017106                           BEFORE INITIAL DOUBLE-SPACE                    
017107                           REPLACING ALL DOTS BY COMMAS                   
017108                                     ALL COLONS BY COMMAS                 
017109                                                                          
017110     MOVE COMMAS             TO DEC-KDSVAR                                
017111     MOVE 7                  TO DEC-KVHELTAL                              
017112     MOVE 2                  TO DEC-KVDECIMAL                             
017113     MOVE TEST-PRARTBEL      TO DEC-IDFRIDATA                             
017114     CALL WDECEDIT        USING DEC-WDECAREA                              
017115                                                                          
017116     IF DEC-KDSVAR-OK                                                     
017117       CONTINUE                                                           
017118     ELSE                                                                 
017123       IF POS > +10                                                       
017124         MOVE NEJ TO INDATA-SW                                            
017125         MOVE 'PRARTBEL > 9 TKN :' TO FELTEXT-STR(IND:18)                 
017126         ADD +18 TO IND                                                   
017127         DISPLAY 'Price is over Max.length '                              
017128                 'for PartNo:' TEST-IDARTNR                               
017129       ELSE                                                               
017130         IF POS > +0                                                      
017131           MOVE ZERO TO INTEGERS                                          
017132           INSPECT TEST-PRARTBEL TALLYING INTEGERS FOR CHARACTERS         
017133                                 BEFORE INITIAL COMMAS                    
017134           IF INTEGERS > +0                                               
017135             IF TEST-PRARTBEL (1:INTEGERS) NOT NUMERIC                    
017136               MOVE NEJ TO INDATA-SW                                      
017137               MOVE 'PRARTBEL NOT NUM :' TO FELTEXT-STR(IND:18)           
017138               ADD +18 TO IND                                             
017139               DISPLAY 'Price Integer part is not numeric '               
017140                       'for PartNo:' TEST-IDARTNR                         
017141             ELSE                                                         
017142               IF POS > INTEGERS                                          
017143               AND TEST-PRARTBEL (INTEGERS + 2:POS - INTEGERS + 1)        
017144                                                      NOT NUMERIC         
017145                 MOVE NEJ TO INDATA-SW                                    
017146                 MOVE 'PRARTBEL NOT NUM :' TO FELTEXT-STR(IND:18)         
017147                 ADD +18 TO IND                                           
017148                 DISPLAY 'Price decimal part is not numeric '             
017149                         'for PartNo:' TEST-IDARTNR                       
017150               ELSE                                                       
017151                 MOVE NEJ TO INDATA-SW                                    
017152                 MOVE 'PRARTBEL NOT OK:' TO FELTEXT-STR(IND:16)           
017153                 ADD +16 TO IND                                           
017154                 DISPLAY 'Price not OK format '                           
017155                          'for PartNo:' TEST-IDARTNR                      
017156               END-IF                                                     
017157             END-IF                                                       
017158           ELSE                                                           
017159*            --- Priset har inga heltal                                   
017160             MOVE NEJ TO INDATA-SW                                        
017161             MOVE 'PRARTBEL HAS NO INTEGERS:'                             
017162                                   TO FELTEXT-STR(IND:25)                 
017163             ADD +25 TO IND                                               
017164             DISPLAY 'Price has no integers '                             
017165                     'for PartNo:' TEST-IDARTNR                           
017166           END-IF                                                         
017167         ELSE                                                             
017168*          --- Pris saknas                                                
017169           MOVE NEJ TO INDATA-SW                                          
017170           MOVE 'PRARTBEL MISSING :' TO FELTEXT-STR(IND:18)               
017171           ADD +18 TO IND                                                 
017172           DISPLAY 'No price declared '                                   
017173                     'for PartNo:' TEST-IDARTNR                           
017174         END-IF                                                           
017175       END-IF                                                             
017176     END-IF                                                               
017180                                                                          
017300***-------------------------                                              
017410                                                                          
017500*    --- PRISLISTE-DATUM                                                  
017510     MOVE ZERO TO POS                                                     
017520     INSPECT TEST-TIPRLIST TALLYING POS FOR CHARACTERS BEFORE             
017530                                            INITIAL '  '                  
017540     IF POS NOT = +8                                                      
017550       MOVE NEJ TO INDATA-SW                                              
017551       MOVE 'TIPRLIST EJ 8 TKN :' TO FELTEXT-STR(IND:19)                  
017552       ADD +19 TO IND                                                     
017553       DISPLAY 'Date has not 8 digits YYYYMMDD '                          
017554               'for PartNo:' TEST-IDARTNR                                 
017560     ELSE                                                                 
017570       IF TEST-TIPRLIST(1:POS) NOT NUMERIC                                
017580         MOVE NEJ TO INDATA-SW                                            
017581         MOVE 'TIPRLIST EJ NUM :' TO FELTEXT-STR(IND:17)                  
017582         ADD +17 TO IND                                                   
017583         DISPLAY 'Date is not numeric '                                   
017584               'for PartNo:' TEST-IDARTNR                                 
017590       ELSE                                                               
017591         IF IN-GCP-TIPRLIST(1:2) NOT = 20                                 
017594           MOVE NEJ TO INDATA-SW                                          
017595           MOVE 'TIPRLIST FEL FORM :' TO FELTEXT-STR(IND:19)              
017596           ADD +19 TO IND                                                 
017597           DISPLAY 'Year must be specified with 4 digits '                
017598                   'for PartNo:' TEST-IDARTNR                             
017601         ELSE                                                             
017602            MOVE IN-GCP-TIPRLIST TO DAT-I-TIDATUM                         
017603            MOVE 'AAMMDD'        TO DAT-KDDATFORM                         
017604            CALL WDATKONV USING DAT-KDDATFORM                             
017605                                DAT-I-TIDATUM                             
017606                                DAT-O-TIDATUM                             
017607                                DAT-KDSVAR                                
017608            IF DAT-KDSVAR-OK                                              
017609              IF IN-GCP-TIPRLIST > DAGENS-DATUM                           
017610                CONTINUE                                                  
017611              ELSE                                                        
017612                MOVE NEJ TO INDATA-SW                                     
017613                MOVE 'TIPRLIST <= DAGENS:' TO FELTEXT-STR(IND:19)         
017614                ADD +19 TO IND                                            
017615                DISPLAY 'Date is not greater than todays date '           
017616                        'for PartNo:' TEST-IDARTNR                        
017619              END-IF                                                      
017620            ELSE                                                          
017621              MOVE NEJ TO INDATA-SW                                       
017622              MOVE 'TIPRLIST EJ DATUM :' TO FELTEXT-STR(IND:19)           
017623              ADD +19 TO IND                                              
017624              DISPLAY 'Not a valid date format '                          
017625                   'for PartNo:' TEST-IDARTNR                             
017628            END-IF                                                        
017629         END-IF                                                           
017630       END-IF                                                             
017631     END-IF                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 C-SKAPA-W55301  SECTION.                                                 
018000     SKIP2                                                                
018100*    -- SKAPA POST TILL W5531200, I W553D2                                
018200*    -- SOM SKAPAR MIDDAR TILL 5111                                       
018300*    -- Indatat är bl.a. kontrollerat mot WDATKONV resp. WDECEDIT         
018310*                                                                         
018400     MOVE '985'             TO W55301-IDPTYP                              
018410                                                                          
018500     MOVE 'BQ8VA'           TO W55301-IDLEVNR                             
018510                                                                          
018600     MOVE IN-GCP-IDARTNR    TO W55301-IDARTNR                             
018610                                                                          
018700*    -- Beställningspriset skall anges i öre !!                           
018710     MULTIPLY DEC-IDEDITDATA BY 100                                       
018720                        GIVING W55301-PRARTBEL                            
018730                                                                          
018800     MOVE '1'               TO W55301-KDANTENH                            
018810                                                                          
018900**   MOVE '0'               TO W55301-KDTIPPR-SI                          
018901     MOVE SPACE             TO W55301-KDFPKPRI                            
018910                                                                          
019000     MOVE DAT-TIAAMMDD      TO W55301-TIPRLIST                            
019010                                                                          
019100     MOVE 'SEK'             TO W55301-KDVALISO                            
019110                                                                          
019200     MOVE ' CLASSIC'        TO W55301-IDUSER                              
019300     .                                                                    
019400     EJECT                                                                
019500 Z-FINIT SECTION.                                                         
019600     CLOSE GCPFIL                                                         
019700           W55301                                                         
019800     SKIP2                                                                
020100     .                                                                    
020200     EJECT                                                                
020300 S01-LAES-GCPFIL  SECTION.                                                
020400     READ GCPFIL INTO IN-AREA                                             
020500     AT END                                                               
020600        MOVE HIGH-VALUE TO IN-AREA                                        
020700        SET END-OF-GCPFIL TO TRUE                                         
020800                                                                          
020900     NOT AT END                                                           
021000        MOVE 'GCPFIL'   TO POSTSUM-FDNAMN                                 
021100        MOVE 'W55301D1' TO POSTSUM-DDNAMN2                                
021200        MOVE SPACE      TO POSTSUM-TRANSTYP                               
021300        CALL POSTSUM USING POSTSUM-PARM                                   
021301                                                                          
021310        ADD +1 TO INDATA-RAKN                                             
021400     END-READ                                                             
021500     .                                                                    
021600     EJECT                                                                
021700 S11-SKRIV-W55301 SECTION.                                                
021800                                                                          
021900     WRITE W55301-POST FROM W55301-AREA                                   
022000                                                                          
022100     MOVE 'UT '         TO POSTSUM-TRANSTYP                               
022200     MOVE 'W55301'      TO POSTSUM-FDNAMN                                 
022300     MOVE 'W55301D2'    TO POSTSUM-DDNAMN2                                
022400     CALL POSTSUM USING POSTSUM-PARM                                      
022500     .                                                                    
022600     EJECT                                                                
022700 S98-STOPPA-FIL-CLOSE-OPEN     SECTION.                                   
022900     SKIP2                                                                
022901*    ----  SLÄNGER REDAN SKRIVNA POSTER.                                  
022902*    ----  Genom att göra CLOSE och sedan OPEN igen.                      
022910     CLOSE W55301                                                         
023000     OPEN OUTPUT W55301                                                   
023300     .                                                                    
023400*S99-ABEND SECTION.                                                       
023500*                                                                         
023600*    SKIP2                                                                
023700*    MOVE 'S' TO POSTSUM-OPKOD                                            
023800*    CALL POSTSUM USING POSTSUM-PARM                                      
023900*    CALL ABEND USING RKOD-ABEND                                          
024000*    .                                                                    
