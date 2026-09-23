000101 ID DIVISION.                                                             
000201     SKIP2                                                                
000301 PROGRAM-ID.     W6115000.                                                
000401*AUTHOR.         LARS THELL.                                              
000501*DATE-WRITTEN.   92/07/01.                                                
000601                                                                          
000701*    REMARKS.                                                             
000801*                                                                         
000901*    FUNKTION:                                                            
001001*        LÄSER FIL MED ALLA INLEVERANS POSTER. SUMMERAR BELÄGGNING        
001101*        PER KDINLUPF OCH SKAPAR FIL MED DESSA UPPGIFTER. DENNA           
001201*        FIL ANVÄNDS SEDAN AV W61152 SOM SKAPAR AK BELÄGGNINGS-           
001301*        LISTA.                                                           
001401*                                                                         
001501*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001601*                                                                         
001701*    ABENDKODER:                                                          
001801*        U0016 -  . . . .                                                 
001901*        U1000 -  TABELLER FULLA ELLER EJ TRÄFF I TABELLER                
002001*                                                                         
002101                                                                          
002201     SKIP3                                                                
002301 ENVIRONMENT DIVISION.                                                    
002401     SKIP2                                                                
002501 INPUT-OUTPUT SECTION.                                                    
002601                                                                          
002701 FILE-CONTROL.                                                            
002801     SKIP2                                                                
002901*          --- INLEVERANS POSTER                                          
003001     SELECT W61110                     ASSIGN TO W61150D1.                
003101     SKIP2                                                                
003201*          --- SUMMAPOSTER BELÄGGNING                                     
003301     SELECT W61150                     ASSIGN TO W61150D2.                
003401     EJECT                                                                
003501 DATA DIVISION.                                                           
003601     SKIP3                                                                
003701 FILE SECTION.                                                            
003801     SKIP3                                                                
003901 FD  W61110                                                               
004001     RECORDING       F                                                    
004101     BLOCK CONTAINS  0.                                                   
004201     SKIP2                                                                
004301*01  -COPY W6111001    -L.                                                
004401     SKIP3                                                                
004501 FD  W61150                                                               
004601     RECORDING       F                                                    
004701     BLOCK CONTAINS  0.                                                   
004801     SKIP2                                                                
004901*01  POST -COPY W6115001 -PRE  UT-  -L.                                   
005001     EJECT                                                                
005101 WORKING-STORAGE SECTION.                                                 
005201     SKIP2                                                                
005301                                                                          
005401*    -- CHECKED BY WY2000                                                 
005501 77  IDPGM                       PIC X(8)    VALUE 'W6115000'.            
005601 77  JA                          PIC X       VALUE 'J'.                   
005701 77  NEJ                         PIC X       VALUE 'N'.                   
005801                                                                          
005901 77  MAX-PLAA-IX                 PIC S9(4)   VALUE +600 COMP SYNC.        
006001 77  MAX-SUM-IX                  PIC S9(4)   VALUE +150 COMP SYNC.        
006101 77  MAX-PK-IX                   PIC S9(4)   VALUE +27  COMP SYNC.        
006201 77  DC-IX                       PIC S9(4)   VALUE +0   COMP SYNC.        
006301                                                                          
006401 77  W-ADINLOMR              PIC X(4)        VALUE SPACE.                 
006501 77  W-KDINLUPF              PIC X(4)        VALUE SPACE.                 
006601                                                                          
006701 77  KDINLUPF-SW                 PIC X       VALUE 'N'.                   
006801     88  KDINLUPF-TRAEFF                     VALUE 'J'.                   
006901                                                                          
007001 77  W61110-EOF-SW               PIC X       VALUE 'N'.                   
007101     88  END-OF-W61110                       VALUE 'J'.                   
007201     EJECT                                                                
007301*      --- VALID IDDC CODES                                               
007401*                                                                         
007501*01    -COPY WWDC99                                                       
007601*01    -COPY WWDCKONS                                                     
007701       EJECT                                                              
007801 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007901 01  FILLER REDEFINES DAGENS-DATUM.                                       
008001     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008101     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008201     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008301     EJECT                                                                
008401 01  DYNAMISKA-SUBPROGRAM.                                                
008501*                                                                         
008601     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008701     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008801     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008901     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009001     SKIP2                                                                
009101*    --- PARAMETRAR TILL ABEND                                            
009201                                                                          
009301 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009401 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009501     SKIP2                                                                
009601 01  FELTEXT.                                                             
009701     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009801     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009901     EJECT                                                                
010001*    --- PARAMETRAR TILL POSTSUM                                          
010101*                                                                         
010201*01  -COPY W0005   -PRE  POSTSUM-                                         
010301     EJECT                                                                
010401 01  IN-AREA-START                PIC X(24)   VALUE                       
010501                                 'IN-AREA-START  '.                       
010601     SKIP2                                                                
010701                                                                          
010801*01  AREA -COPY W6111001   -PRE IN-                                       
010901     EJECT                                                                
011001 01  UT-AREA-START                PIC X(24)   VALUE                       
011101                                 'UT-AREA-START  '.                       
011201     SKIP2                                                                
011301                                                                          
011401*01  AREA -COPY W6115001   -PRE UT-                                       
011501     EJECT                                                                
011601*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011701*                                                                         
011801 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011901     SKIP3                                                                
012001 01  NYCKLAR-TILL-DLI.                                                    
012101     03  W-W6GXKEY-6005-X.                                                
012201         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
012301         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
012401         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
012501     SKIP2                                                                
012601*    --- STATUS-KOD FRÅN IMS                                              
012701 01  STATUS-WS                   PIC XX.                                  
012801     88  SEGMENT-FINNS                       VALUE '  '.                  
012901     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013001     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013101     SKIP2                                                                
013201 01  GODK-STATUSKODER.                                                    
013301     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013401     SKIP3                                                                
013501 01  SSA1                        PIC X(64).                               
013601 01  SSA2                        PIC X(64).                               
013701     EJECT                                                                
013801*    --- IMS FUNKTIONSKODER                                               
013901*01  -COPY W0003                                                          
014001     EJECT                                                                
014101*    ---  DLI INPUT-OUTPUT AREA                                           
014201 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014301     SKIP3                                                                
014401 01  DLI-IO-AREA.                                                         
014501     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
014601     SKIP3                                                                
014701     03  W6PLAA11 REDEFINES IO-AREA.                                      
014801*        05  -COPY W6GX6006 -PRE PLAA-                                    
014901     EJECT                                                                
015001                                                                          
015101*    --- TABELL FÖR PLACERINGS-REGISTRET PER DC                           
015201 01  FILLER.                                                              
015301     03 W-PLAA-IDDC-TAB  OCCURS 27  INDEXED BY IDDC-IX.                   
015401       05 W-PLAA-TAB     OCCURS 600 INDEXED BY PLAA-IX.                   
015501          07  W-PLAA-TAB-ADINLOMR     PIC X(4).                           
015601          07  W-PLAA-TAB-KDINLUPF     PIC X(4).                           
015701                                                                          
015801*    --- TABELL FÖR SUMMERINGAR PER KDINLUPF OCH DC                       
015901 01  FILLER.                                                              
016001     03 W-SUM-IDDC       OCCURS 27.                                       
016101       05 W-SUM-TAB      OCCURS 150 INDEXED BY SUM-IX.                    
016201          07  W-SUM-KDINLUPF          PIC X(4).                           
016301          07  W-SUM-KVART             PIC S9(7)      COMP-3.              
016401          07  W-SUM-KVRADER           PIC S9(5)      COMP-3.              
016501          07  W-SUM-KVRADER-PRIO      PIC S9(5)      COMP-3.              
016601          07  W-SUM-KVKOLLI           PIC S9(5)      COMP-3.              
016701          07  W-SUM-KVKOLLI-PRIO      PIC S9(5)      COMP-3.              
016801          07  W-SUM-SUBEL             PIC S9(9)V9(2) COMP-3.              
016901          07  W-SUM-SUBEL-PRIO        PIC S9(9)V9(2) COMP-3.              
017001                                                                          
017101*    --- TABELL FÖR KDINLUPF PER PARTI OCH DC                             
017201 01  FILLER.                                                              
017301     03 W-PK-IDDC         OCCURS 27.                                      
017401       05 W-PARTI-TAB     OCCURS 27 INDEXED BY PK-IX.                     
017501          07  W-PARTI-KDINLUPF          PIC X(4).                         
017601     SKIP2                                                                
017701*    --- TABELL FÖR TOTALSIFFROR PER DC                                   
017801 01  FILLER.                                                              
017901     03 W-TOT-IDDC         OCCURS 27.                                     
018001       05  W-TOT-KVART           PIC S9(7)       VALUE ZERO COMP.         
018101       05  W-TOT-KVRADER         PIC S9(5)       VALUE ZERO COMP.         
018201       05  W-TOT-KVRADER-PRIO    PIC S9(5)       VALUE ZERO COMP.         
018301       05  W-TOT-KVKOLLI         PIC S9(5)       VALUE ZERO COMP.         
018401       05  W-TOT-KVKOLLI-PRIO    PIC S9(5)       VALUE ZERO COMP.         
018501       05  W-TOT-SUBEL           PIC S9(9)V9(2)  VALUE ZERO COMP.         
018601       05  W-TOT-SUBEL-PRIO      PIC S9(9)V9(2)  VALUE ZERO COMP.         
018701       05  W-OLD-IDLOPNRM        PIC S9(9)       VALUE ZERO COMP.         
018801     EJECT                                                                
018901 LINKAGE SECTION.                                                         
019001                                                                          
019101*01  -COPY W0008  -PRE PLAA-                                              
019201     05  FILLER                  PIC X.                                   
019301     EJECT                                                                
019401 PROCEDURE DIVISION  USING PLAA-PCB.                                      
019501     ENTRY 'DLITCBL' USING PLAA-PCB.                                      
019601                                                                          
019701     SKIP2                                                                
019801     PERFORM A-INIT                                                       
019901     PERFORM B-LAES-IN-PLAA-BAADA-DC                                      
020001                                                                          
020101     PERFORM S01-LAES-W61110                                              
020201     PERFORM UNTIL END-OF-W61110                                          
020301       IF IN-KDINLSTA = SPACE OR 'FPK'                                    
020401          PERFORM C-BEHANDLA-INLEVERANSPOST                               
020501       END-IF                                                             
020601       PERFORM S01-LAES-W61110                                            
020701     END-PERFORM                                                          
020801                                                                          
020901     PERFORM D-TOEM-KDINLUPF-TAB                                          
021001     PERFORM E-SKRIV-TOTAL-POST                                           
021101                                                                          
021201     PERFORM Z-FINIT                                                      
021301                                                                          
021401     MOVE ZERO TO RETURN-CODE                                             
021501     GOBACK                                                               
021601     .                                                                    
021701     EJECT                                                                
021801 A-INIT SECTION.                                                          
021901                                                                          
022001     OPEN INPUT  W61110                                                   
022101                                                                          
022201     OPEN OUTPUT W61150                                                   
022301     SKIP2                                                                
022401     ACCEPT DAGENS-DATUM  FROM DATE                                       
022501     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022601                                                                          
022701     SET PLAA-IX               TO  +1                                     
022801     PERFORM UNTIL PLAA-IX     >   MAX-PLAA-IX                            
022901       MOVE SPACE            TO  W-PLAA-TAB-ADINLOMR(1 PLAA-IX)           
023001                                 W-PLAA-TAB-ADINLOMR(2 PLAA-IX)           
023101                                 W-PLAA-TAB-ADINLOMR(3 PLAA-IX)           
023201                                 W-PLAA-TAB-ADINLOMR(4 PLAA-IX)           
023301                                 W-PLAA-TAB-ADINLOMR(5 PLAA-IX)           
023401                                 W-PLAA-TAB-ADINLOMR(6 PLAA-IX)           
023501                                 W-PLAA-TAB-ADINLOMR(7 PLAA-IX)           
023601                                 W-PLAA-TAB-ADINLOMR(8 PLAA-IX)           
023701                                 W-PLAA-TAB-ADINLOMR(9 PLAA-IX)           
023801                                 W-PLAA-TAB-ADINLOMR(10 PLAA-IX)          
023901                                 W-PLAA-TAB-ADINLOMR(11 PLAA-IX)          
024001                                 W-PLAA-TAB-ADINLOMR(12 PLAA-IX)          
024101                                 W-PLAA-TAB-ADINLOMR(13 PLAA-IX)          
024201                                 W-PLAA-TAB-ADINLOMR(14 PLAA-IX)          
024301                                 W-PLAA-TAB-ADINLOMR(15 PLAA-IX)          
024401                                 W-PLAA-TAB-ADINLOMR(16 PLAA-IX)          
024402                                 W-PLAA-TAB-ADINLOMR(17 PLAA-IX)          
024403                                 W-PLAA-TAB-ADINLOMR(18 PLAA-IX)          
024404                                 W-PLAA-TAB-ADINLOMR(19 PLAA-IX)          
024405                                 W-PLAA-TAB-ADINLOMR(20 PLAA-IX)          
024406                                 W-PLAA-TAB-ADINLOMR(21 PLAA-IX)          
024407                                 W-PLAA-TAB-ADINLOMR(22 PLAA-IX)          
024408                                 W-PLAA-TAB-ADINLOMR(23 PLAA-IX)          
024409                                 W-PLAA-TAB-ADINLOMR(24 PLAA-IX)          
024410                                 W-PLAA-TAB-ADINLOMR(25 PLAA-IX)          
024411                                 W-PLAA-TAB-ADINLOMR(26 PLAA-IX)          
024412                                 W-PLAA-TAB-ADINLOMR(27 PLAA-IX)          
024501                                 W-PLAA-TAB-KDINLUPF(1 PLAA-IX)           
024601                                 W-PLAA-TAB-KDINLUPF(2 PLAA-IX)           
024701                                 W-PLAA-TAB-KDINLUPF(3 PLAA-IX)           
024801                                 W-PLAA-TAB-KDINLUPF(4 PLAA-IX)           
024901                                 W-PLAA-TAB-KDINLUPF(5 PLAA-IX)           
025001                                 W-PLAA-TAB-KDINLUPF(6 PLAA-IX)           
025101                                 W-PLAA-TAB-KDINLUPF(7 PLAA-IX)           
025201                                 W-PLAA-TAB-KDINLUPF(8 PLAA-IX)           
025301                                 W-PLAA-TAB-KDINLUPF(9 PLAA-IX)           
025401                                 W-PLAA-TAB-KDINLUPF(10 PLAA-IX)          
025501                                 W-PLAA-TAB-KDINLUPF(11 PLAA-IX)          
025601                                 W-PLAA-TAB-KDINLUPF(12 PLAA-IX)          
025701                                 W-PLAA-TAB-KDINLUPF(13 PLAA-IX)          
025801                                 W-PLAA-TAB-KDINLUPF(14 PLAA-IX)          
025901                                 W-PLAA-TAB-KDINLUPF(15 PLAA-IX)          
026001                                 W-PLAA-TAB-KDINLUPF(16 PLAA-IX)          
026002                                 W-PLAA-TAB-KDINLUPF(17 PLAA-IX)          
026003                                 W-PLAA-TAB-KDINLUPF(18 PLAA-IX)          
026004                                 W-PLAA-TAB-KDINLUPF(19 PLAA-IX)          
026005                                 W-PLAA-TAB-KDINLUPF(20 PLAA-IX)          
026006                                 W-PLAA-TAB-KDINLUPF(21 PLAA-IX)          
026007                                 W-PLAA-TAB-KDINLUPF(22 PLAA-IX)          
026008                                 W-PLAA-TAB-KDINLUPF(23 PLAA-IX)          
026009                                 W-PLAA-TAB-KDINLUPF(24 PLAA-IX)          
026010                                 W-PLAA-TAB-KDINLUPF(25 PLAA-IX)          
026020                                 W-PLAA-TAB-KDINLUPF(26 PLAA-IX)          
026030                                 W-PLAA-TAB-KDINLUPF(27 PLAA-IX)          
026101       SET PLAA-IX UP BY +1                                               
026201     END-PERFORM                                                          
026301                                                                          
026401     SET SUM-IX                TO +1                                      
026501     PERFORM UNTIL SUM-IX      >  MAX-SUM-IX                              
026601         MOVE SPACE            TO W-SUM-KDINLUPF    (1 SUM-IX)            
026701                                  W-SUM-KDINLUPF    (2 SUM-IX)            
026801                                  W-SUM-KDINLUPF    (3 SUM-IX)            
026901                                  W-SUM-KDINLUPF    (4 SUM-IX)            
027001                                  W-SUM-KDINLUPF    (5 SUM-IX)            
027101                                  W-SUM-KDINLUPF    (6 SUM-IX)            
027201                                  W-SUM-KDINLUPF    (7 SUM-IX)            
027301                                  W-SUM-KDINLUPF    (8 SUM-IX)            
027401                                  W-SUM-KDINLUPF    (9 SUM-IX)            
027501                                  W-SUM-KDINLUPF    (10 SUM-IX)           
027601                                  W-SUM-KDINLUPF    (11 SUM-IX)           
027701                                  W-SUM-KDINLUPF    (12 SUM-IX)           
027801                                  W-SUM-KDINLUPF    (13 SUM-IX)           
027901                                  W-SUM-KDINLUPF    (14 SUM-IX)           
028001                                  W-SUM-KDINLUPF    (15 SUM-IX)           
028101                                  W-SUM-KDINLUPF    (16 SUM-IX)           
028102                                  W-SUM-KDINLUPF    (17 SUM-IX)           
028103                                  W-SUM-KDINLUPF    (18 SUM-IX)           
028104                                  W-SUM-KDINLUPF    (19 SUM-IX)           
028105                                  W-SUM-KDINLUPF    (20 SUM-IX)           
028106                                  W-SUM-KDINLUPF    (21 SUM-IX)           
028107                                  W-SUM-KDINLUPF    (22 SUM-IX)           
028108                                  W-SUM-KDINLUPF    (23 SUM-IX)           
028109                                  W-SUM-KDINLUPF    (24 SUM-IX)           
028110                                  W-SUM-KDINLUPF    (25 SUM-IX)           
028120                                  W-SUM-KDINLUPF    (26 SUM-IX)           
028130                                  W-SUM-KDINLUPF    (27 SUM-IX)           
028201         MOVE ZERO             TO W-SUM-KVART       (1 SUM-IX)            
028301                                  W-SUM-KVART       (2 SUM-IX)            
028401                                  W-SUM-KVART       (3 SUM-IX)            
028501                                  W-SUM-KVART       (4 SUM-IX)            
028601                                  W-SUM-KVART       (5 SUM-IX)            
028701                                  W-SUM-KVART       (6 SUM-IX)            
028801                                  W-SUM-KVART       (7 SUM-IX)            
028901                                  W-SUM-KVART       (8 SUM-IX)            
029001                                  W-SUM-KVART       (9 SUM-IX)            
029101                                  W-SUM-KVART       (10 SUM-IX)           
029201                                  W-SUM-KVART       (11 SUM-IX)           
029301                                  W-SUM-KVART       (12 SUM-IX)           
029401                                  W-SUM-KVART       (13 SUM-IX)           
029501                                  W-SUM-KVART       (14 SUM-IX)           
029601                                  W-SUM-KVART       (15 SUM-IX)           
029701                                  W-SUM-KVART       (16 SUM-IX)           
029702                                  W-SUM-KVART       (17 SUM-IX)           
029703                                  W-SUM-KVART       (18 SUM-IX)           
029704                                  W-SUM-KVART       (19 SUM-IX)           
029705                                  W-SUM-KVART       (20 SUM-IX)           
029706                                  W-SUM-KVART       (21 SUM-IX)           
029707                                  W-SUM-KVART       (22 SUM-IX)           
029708                                  W-SUM-KVART       (23 SUM-IX)           
029709                                  W-SUM-KVART       (24 SUM-IX)           
029710                                  W-SUM-KVART       (25 SUM-IX)           
029720                                  W-SUM-KVART       (26 SUM-IX)           
029730                                  W-SUM-KVART       (27 SUM-IX)           
029801                                  W-SUM-KVKOLLI     (1 SUM-IX)            
029901                                  W-SUM-KVKOLLI     (2 SUM-IX)            
030001                                  W-SUM-KVKOLLI     (3 SUM-IX)            
030101                                  W-SUM-KVKOLLI     (4 SUM-IX)            
030201                                  W-SUM-KVKOLLI     (5 SUM-IX)            
030301                                  W-SUM-KVKOLLI     (6 SUM-IX)            
030401                                  W-SUM-KVKOLLI     (7 SUM-IX)            
030501                                  W-SUM-KVKOLLI     (8 SUM-IX)            
030601                                  W-SUM-KVKOLLI     (9 SUM-IX)            
030701                                  W-SUM-KVKOLLI     (10 SUM-IX)           
030801                                  W-SUM-KVKOLLI     (11 SUM-IX)           
030901                                  W-SUM-KVKOLLI     (12 SUM-IX)           
031001                                  W-SUM-KVKOLLI     (13 SUM-IX)           
031101                                  W-SUM-KVKOLLI     (14 SUM-IX)           
031201                                  W-SUM-KVKOLLI     (15 SUM-IX)           
031301                                  W-SUM-KVKOLLI     (16 SUM-IX)           
031302                                  W-SUM-KVKOLLI     (17 SUM-IX)           
031303                                  W-SUM-KVKOLLI     (18 SUM-IX)           
031304                                  W-SUM-KVKOLLI     (19 SUM-IX)           
031305                                  W-SUM-KVKOLLI     (20 SUM-IX)           
031306                                  W-SUM-KVKOLLI     (21 SUM-IX)           
031307                                  W-SUM-KVKOLLI     (22 SUM-IX)           
031308                                  W-SUM-KVKOLLI     (23 SUM-IX)           
031309                                  W-SUM-KVKOLLI     (24 SUM-IX)           
031310                                  W-SUM-KVKOLLI     (25 SUM-IX)           
031320                                  W-SUM-KVKOLLI     (26 SUM-IX)           
031330                                  W-SUM-KVKOLLI     (27 SUM-IX)           
031401                                  W-SUM-KVKOLLI-PRIO(1 SUM-IX)            
031501                                  W-SUM-KVKOLLI-PRIO(2 SUM-IX)            
031601                                  W-SUM-KVKOLLI-PRIO(3 SUM-IX)            
031701                                  W-SUM-KVKOLLI-PRIO(4 SUM-IX)            
031801                                  W-SUM-KVKOLLI-PRIO(5 SUM-IX)            
031901                                  W-SUM-KVKOLLI-PRIO(6 SUM-IX)            
032001                                  W-SUM-KVKOLLI-PRIO(7 SUM-IX)            
032101                                  W-SUM-KVKOLLI-PRIO(8 SUM-IX)            
032201                                  W-SUM-KVKOLLI-PRIO(9 SUM-IX)            
032301                                  W-SUM-KVKOLLI-PRIO(10 SUM-IX)           
032401                                  W-SUM-KVKOLLI-PRIO(11 SUM-IX)           
032501                                  W-SUM-KVKOLLI-PRIO(12 SUM-IX)           
032601                                  W-SUM-KVKOLLI-PRIO(13 SUM-IX)           
032701                                  W-SUM-KVKOLLI-PRIO(14 SUM-IX)           
032801                                  W-SUM-KVKOLLI-PRIO(15 SUM-IX)           
032901                                  W-SUM-KVKOLLI-PRIO(16 SUM-IX)           
032902                                  W-SUM-KVKOLLI-PRIO(17 SUM-IX)           
032903                                  W-SUM-KVKOLLI-PRIO(18 SUM-IX)           
032904                                  W-SUM-KVKOLLI-PRIO(19 SUM-IX)           
032905                                  W-SUM-KVKOLLI-PRIO(20 SUM-IX)           
032906                                  W-SUM-KVKOLLI-PRIO(21 SUM-IX)           
032907                                  W-SUM-KVKOLLI-PRIO(22 SUM-IX)           
032908                                  W-SUM-KVKOLLI-PRIO(23 SUM-IX)           
032909                                  W-SUM-KVKOLLI-PRIO(24 SUM-IX)           
032910                                  W-SUM-KVKOLLI-PRIO(25 SUM-IX)           
032920                                  W-SUM-KVKOLLI-PRIO(26 SUM-IX)           
032930                                  W-SUM-KVKOLLI-PRIO(27 SUM-IX)           
033001                                  W-SUM-KVRADER     (1 SUM-IX)            
033101                                  W-SUM-KVRADER     (2 SUM-IX)            
033201                                  W-SUM-KVRADER     (3 SUM-IX)            
033301                                  W-SUM-KVRADER     (4 SUM-IX)            
033401                                  W-SUM-KVRADER     (5 SUM-IX)            
033501                                  W-SUM-KVRADER     (6 SUM-IX)            
033601                                  W-SUM-KVRADER     (7 SUM-IX)            
033701                                  W-SUM-KVRADER     (8 SUM-IX)            
033801                                  W-SUM-KVRADER     (9 SUM-IX)            
033901                                  W-SUM-KVRADER     (10 SUM-IX)           
034001                                  W-SUM-KVRADER     (11 SUM-IX)           
034101                                  W-SUM-KVRADER     (12 SUM-IX)           
034201                                  W-SUM-KVRADER     (13 SUM-IX)           
034301                                  W-SUM-KVRADER     (14 SUM-IX)           
034401                                  W-SUM-KVRADER     (15 SUM-IX)           
034501                                  W-SUM-KVRADER     (16 SUM-IX)           
034502                                  W-SUM-KVRADER     (17 SUM-IX)           
034503                                  W-SUM-KVRADER     (18 SUM-IX)           
034504                                  W-SUM-KVRADER     (19 SUM-IX)           
034505                                  W-SUM-KVRADER     (20 SUM-IX)           
034506                                  W-SUM-KVRADER     (21 SUM-IX)           
034507                                  W-SUM-KVRADER     (22 SUM-IX)           
034508                                  W-SUM-KVRADER     (23 SUM-IX)           
034509                                  W-SUM-KVRADER     (24 SUM-IX)           
034510                                  W-SUM-KVRADER     (25 SUM-IX)           
034520                                  W-SUM-KVRADER     (26 SUM-IX)           
034530                                  W-SUM-KVRADER     (27 SUM-IX)           
034601                                  W-SUM-KVRADER-PRIO(1 SUM-IX)            
034701                                  W-SUM-KVRADER-PRIO(2 SUM-IX)            
034801                                  W-SUM-KVRADER-PRIO(3 SUM-IX)            
034901                                  W-SUM-KVRADER-PRIO(4 SUM-IX)            
035001                                  W-SUM-KVRADER-PRIO(5 SUM-IX)            
035101                                  W-SUM-KVRADER-PRIO(6 SUM-IX)            
035201                                  W-SUM-KVRADER-PRIO(7 SUM-IX)            
035301                                  W-SUM-KVRADER-PRIO(8 SUM-IX)            
035401                                  W-SUM-KVRADER-PRIO(9 SUM-IX)            
035501                                  W-SUM-KVRADER-PRIO(10 SUM-IX)           
035601                                  W-SUM-KVRADER-PRIO(11 SUM-IX)           
035701                                  W-SUM-KVRADER-PRIO(12 SUM-IX)           
035801                                  W-SUM-KVRADER-PRIO(13 SUM-IX)           
035901                                  W-SUM-KVRADER-PRIO(14 SUM-IX)           
036001                                  W-SUM-KVRADER-PRIO(15 SUM-IX)           
036101                                  W-SUM-KVRADER-PRIO(16 SUM-IX)           
036102                                  W-SUM-KVRADER-PRIO(17 SUM-IX)           
036103                                  W-SUM-KVRADER-PRIO(18 SUM-IX)           
036104                                  W-SUM-KVRADER-PRIO(19 SUM-IX)           
036105                                  W-SUM-KVRADER-PRIO(20 SUM-IX)           
036106                                  W-SUM-KVRADER-PRIO(21 SUM-IX)           
036107                                  W-SUM-KVRADER-PRIO(22 SUM-IX)           
036108                                  W-SUM-KVRADER-PRIO(23 SUM-IX)           
036109                                  W-SUM-KVRADER-PRIO(24 SUM-IX)           
036110                                  W-SUM-KVRADER-PRIO(25 SUM-IX)           
036120                                  W-SUM-KVRADER-PRIO(26 SUM-IX)           
036130                                  W-SUM-KVRADER-PRIO(27 SUM-IX)           
036201                                  W-SUM-SUBEL       (1 SUM-IX)            
036301                                  W-SUM-SUBEL       (2 SUM-IX)            
036401                                  W-SUM-SUBEL       (3 SUM-IX)            
036501                                  W-SUM-SUBEL       (4 SUM-IX)            
036601                                  W-SUM-SUBEL       (5 SUM-IX)            
036701                                  W-SUM-SUBEL       (6 SUM-IX)            
036801                                  W-SUM-SUBEL       (7 SUM-IX)            
036901                                  W-SUM-SUBEL       (8 SUM-IX)            
037001                                  W-SUM-SUBEL       (9 SUM-IX)            
037101                                  W-SUM-SUBEL       (10 SUM-IX)           
037201                                  W-SUM-SUBEL       (11 SUM-IX)           
037301                                  W-SUM-SUBEL       (12 SUM-IX)           
037401                                  W-SUM-SUBEL       (13 SUM-IX)           
037501                                  W-SUM-SUBEL       (14 SUM-IX)           
037601                                  W-SUM-SUBEL       (15 SUM-IX)           
037701                                  W-SUM-SUBEL       (16 SUM-IX)           
037702                                  W-SUM-SUBEL       (17 SUM-IX)           
037703                                  W-SUM-SUBEL       (18 SUM-IX)           
037704                                  W-SUM-SUBEL       (19 SUM-IX)           
037705                                  W-SUM-SUBEL       (20 SUM-IX)           
037706                                  W-SUM-SUBEL       (21 SUM-IX)           
037707                                  W-SUM-SUBEL       (22 SUM-IX)           
037708                                  W-SUM-SUBEL       (23 SUM-IX)           
037709                                  W-SUM-SUBEL       (24 SUM-IX)           
037710                                  W-SUM-SUBEL       (25 SUM-IX)           
037720                                  W-SUM-SUBEL       (26 SUM-IX)           
037730                                  W-SUM-SUBEL       (27 SUM-IX)           
037801                                  W-SUM-SUBEL-PRIO  (1 SUM-IX)            
037901                                  W-SUM-SUBEL-PRIO  (2 SUM-IX)            
038001                                  W-SUM-SUBEL-PRIO  (3 SUM-IX)            
038101                                  W-SUM-SUBEL-PRIO  (4 SUM-IX)            
038201                                  W-SUM-SUBEL-PRIO  (5 SUM-IX)            
038301                                  W-SUM-SUBEL-PRIO  (6 SUM-IX)            
038401                                  W-SUM-SUBEL-PRIO  (7 SUM-IX)            
038501                                  W-SUM-SUBEL-PRIO  (8 SUM-IX)            
038601                                  W-SUM-SUBEL-PRIO  (9 SUM-IX)            
038701                                  W-SUM-SUBEL-PRIO  (10 SUM-IX)           
038801                                  W-SUM-SUBEL-PRIO  (11 SUM-IX)           
038901                                  W-SUM-SUBEL-PRIO  (12 SUM-IX)           
039001                                  W-SUM-SUBEL-PRIO  (13 SUM-IX)           
039101                                  W-SUM-SUBEL-PRIO  (14 SUM-IX)           
039201                                  W-SUM-SUBEL-PRIO  (15 SUM-IX)           
039301                                  W-SUM-SUBEL-PRIO  (16 SUM-IX)           
039302                                  W-SUM-SUBEL-PRIO  (17 SUM-IX)           
039303                                  W-SUM-SUBEL-PRIO  (18 SUM-IX)           
039304                                  W-SUM-SUBEL-PRIO  (19 SUM-IX)           
039305                                  W-SUM-SUBEL-PRIO  (20 SUM-IX)           
039306                                  W-SUM-SUBEL-PRIO  (21 SUM-IX)           
039307                                  W-SUM-SUBEL-PRIO  (22 SUM-IX)           
039308                                  W-SUM-SUBEL-PRIO  (23 SUM-IX)           
039309                                  W-SUM-SUBEL-PRIO  (24 SUM-IX)           
039310                                  W-SUM-SUBEL-PRIO  (25 SUM-IX)           
039320                                  W-SUM-SUBEL-PRIO  (26 SUM-IX)           
039330                                  W-SUM-SUBEL-PRIO  (27 SUM-IX)           
039401         SET SUM-IX UP BY +1                                              
039501     END-PERFORM                                                          
039601                                                                          
039701     SET PK-IX                 TO +1                                      
039801     PERFORM UNTIL PK-IX       >  MAX-PK-IX                               
039901         MOVE SPACE            TO W-PARTI-KDINLUPF   (1 PK-IX)            
040001                                  W-PARTI-KDINLUPF   (2 PK-IX)            
040101                                  W-PARTI-KDINLUPF   (3 PK-IX)            
040201                                  W-PARTI-KDINLUPF   (4 PK-IX)            
040301                                  W-PARTI-KDINLUPF   (5 PK-IX)            
040401                                  W-PARTI-KDINLUPF   (6 PK-IX)            
040501                                  W-PARTI-KDINLUPF   (7 PK-IX)            
040601                                  W-PARTI-KDINLUPF   (8 PK-IX)            
040701                                  W-PARTI-KDINLUPF   (9 PK-IX)            
040801                                  W-PARTI-KDINLUPF   (10 PK-IX)           
040901                                  W-PARTI-KDINLUPF   (11 PK-IX)           
041001                                  W-PARTI-KDINLUPF   (12 PK-IX)           
041101                                  W-PARTI-KDINLUPF   (13 PK-IX)           
041201                                  W-PARTI-KDINLUPF   (14 PK-IX)           
041301                                  W-PARTI-KDINLUPF   (15 PK-IX)           
041401                                  W-PARTI-KDINLUPF   (16 PK-IX)           
041402                                  W-PARTI-KDINLUPF   (17 PK-IX)           
041403                                  W-PARTI-KDINLUPF   (18 PK-IX)           
041404                                  W-PARTI-KDINLUPF   (19 PK-IX)           
041405                                  W-PARTI-KDINLUPF   (20 PK-IX)           
041406                                  W-PARTI-KDINLUPF   (21 PK-IX)           
041407                                  W-PARTI-KDINLUPF   (22 PK-IX)           
041408                                  W-PARTI-KDINLUPF   (23 PK-IX)           
041409                                  W-PARTI-KDINLUPF   (24 PK-IX)           
041410                                  W-PARTI-KDINLUPF   (25 PK-IX)           
041420                                  W-PARTI-KDINLUPF   (26 PK-IX)           
041430                                  W-PARTI-KDINLUPF   (27 PK-IX)           
041501         SET PK-IX UP BY +1                                               
041601     END-PERFORM                                                          
041701                                                                          
041801     MOVE +1 TO DC-IX                                                     
041901     PERFORM UNTIL DC-IX > MAX-PK-IX                                      
042001       MOVE ZERO  TO W-TOT-KVART        (DC-IX)                           
042101                     W-TOT-KVRADER      (DC-IX)                           
042201                     W-TOT-KVRADER-PRIO (DC-IX)                           
042301                     W-TOT-KVKOLLI      (DC-IX)                           
042401                     W-TOT-KVKOLLI-PRIO (DC-IX)                           
042501                     W-TOT-SUBEL        (DC-IX)                           
042601                     W-TOT-SUBEL-PRIO   (DC-IX)                           
042701                     W-OLD-IDLOPNRM     (DC-IX)                           
042801         ADD +1    TO DC-IX                                               
042901     END-PERFORM                                                          
043001     .                                                                    
043101     EJECT                                                                
043201 B-LAES-IN-PLAA-BAADA-DC   SECTION.                                       
043301                                                                          
043401     SET  PLAA-IX              TO +1                                      
043501     MOVE 'R31 '               TO W-PLAA-TAB-KDINLUPF (1 PLAA-IX)         
043601                                  W-PLAA-TAB-KDINLUPF (2 PLAA-IX)         
043701                                  W-PLAA-TAB-KDINLUPF (3 PLAA-IX)         
043801                                  W-PLAA-TAB-KDINLUPF (4 PLAA-IX)         
043901                                  W-PLAA-TAB-KDINLUPF (5 PLAA-IX)         
044001                                  W-PLAA-TAB-KDINLUPF (6 PLAA-IX)         
044101                                  W-PLAA-TAB-KDINLUPF (7 PLAA-IX)         
044201                                  W-PLAA-TAB-KDINLUPF (8 PLAA-IX)         
044301                                  W-PLAA-TAB-KDINLUPF (9 PLAA-IX)         
044401                                  W-PLAA-TAB-KDINLUPF (10 PLAA-IX)        
044501                                  W-PLAA-TAB-KDINLUPF (11 PLAA-IX)        
044601                                  W-PLAA-TAB-KDINLUPF (12 PLAA-IX)        
044701                                  W-PLAA-TAB-KDINLUPF (13 PLAA-IX)        
044801                                  W-PLAA-TAB-KDINLUPF (14 PLAA-IX)        
044901                                  W-PLAA-TAB-KDINLUPF (15 PLAA-IX)        
045001                                  W-PLAA-TAB-KDINLUPF (16 PLAA-IX)        
045002                                  W-PLAA-TAB-KDINLUPF (17 PLAA-IX)        
045003                                  W-PLAA-TAB-KDINLUPF (18 PLAA-IX)        
045004                                  W-PLAA-TAB-KDINLUPF (19 PLAA-IX)        
045005                                  W-PLAA-TAB-KDINLUPF (20 PLAA-IX)        
045006                                  W-PLAA-TAB-KDINLUPF (21 PLAA-IX)        
045007                                  W-PLAA-TAB-KDINLUPF (22 PLAA-IX)        
045008                                  W-PLAA-TAB-KDINLUPF (23 PLAA-IX)        
045009                                  W-PLAA-TAB-KDINLUPF (24 PLAA-IX)        
045010                                  W-PLAA-TAB-KDINLUPF (25 PLAA-IX)        
045020                                  W-PLAA-TAB-KDINLUPF (26 PLAA-IX)        
045030                                  W-PLAA-TAB-KDINLUPF (27 PLAA-IX)        
045101                                                                          
045201     SET  PLAA-IX              TO +2                                      
045301     MOVE 'C2  '               TO W-PLAA-TAB-KDINLUPF (1 PLAA-IX)         
045401                                                                          
045501     SET  PLAA-IX              TO +3                                      
045601     MOVE WC-CDC-SE TO W-6005-IDDC                                        
045701     PERFORM IMS-GU-PLAA-PLAA11                                           
045801     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
045901         MOVE PLAA-6006-ADINLOMR TO W-PLAA-TAB-ADINLOMR(1 PLAA-IX)        
046001         MOVE PLAA-6006-KDINLUPF TO W-PLAA-TAB-KDINLUPF(1 PLAA-IX)        
046101         SET PLAA-IX UP BY +1                                             
046201         PERFORM IMS-GNP-PLAA-PLAA11                                      
046301     END-PERFORM                                                          
046401                                                                          
046501     IF PLAA-IX                   > MAX-PLAA-IX                           
046601         MOVE 'PLAA TABELL FULL FÖR DC=11 ' TO FELTEXT-STR                
046701         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
046801     END-IF                                                               
046901                                                                          
047001     SET  PLAA-IX              TO +2                                      
047101     MOVE WC-CDC-TR TO W-6005-IDDC                                        
047201     PERFORM IMS-GU-PLAA-PLAA11                                           
047301     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
047401         MOVE PLAA-6006-ADINLOMR TO W-PLAA-TAB-ADINLOMR(2 PLAA-IX)        
047501         MOVE PLAA-6006-KDINLUPF TO W-PLAA-TAB-KDINLUPF(2 PLAA-IX)        
047601         SET PLAA-IX UP BY +1                                             
047701         PERFORM IMS-GNP-PLAA-PLAA11                                      
047801     END-PERFORM                                                          
047901                                                                          
048001     IF PLAA-IX                   > MAX-PLAA-IX                           
048101         MOVE 'PLAA TABELL FULL FÖR DC=12 ' TO FELTEXT-STR                
048201         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
048301     END-IF                                                               
048401                                                                          
048501     SET  PLAA-IX              TO +2                                      
048601     MOVE WC-NDC-US-RU TO W-6005-IDDC                                     
048701     PERFORM IMS-GU-PLAA-PLAA11                                           
048801     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
048901         MOVE PLAA-6006-ADINLOMR TO W-PLAA-TAB-ADINLOMR(3 PLAA-IX)        
049001         MOVE PLAA-6006-KDINLUPF TO W-PLAA-TAB-KDINLUPF(3 PLAA-IX)        
049101         SET PLAA-IX UP BY +1                                             
049201         PERFORM IMS-GNP-PLAA-PLAA11                                      
049301     END-PERFORM                                                          
049401                                                                          
049501     IF PLAA-IX                   > MAX-PLAA-IX                           
049601         MOVE 'PLAA TABELL FULL FÖR DC=41 ' TO FELTEXT-STR                
049701         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
049801     END-IF                                                               
049901                                                                          
050001     SET  PLAA-IX              TO +2                                      
050101     MOVE WC-NDC-US-BAT TO W-6005-IDDC                                    
050201     PERFORM IMS-GU-PLAA-PLAA11                                           
050301     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
050401         MOVE PLAA-6006-ADINLOMR TO W-PLAA-TAB-ADINLOMR(4 PLAA-IX)        
050501         MOVE PLAA-6006-KDINLUPF TO W-PLAA-TAB-KDINLUPF(4 PLAA-IX)        
050601         SET PLAA-IX UP BY +1                                             
050701         PERFORM IMS-GNP-PLAA-PLAA11                                      
050801     END-PERFORM                                                          
050901                                                                          
051001     IF PLAA-IX                   > MAX-PLAA-IX                           
051101         MOVE 'PLAA TABELL FULL FÖR DC=92 ' TO FELTEXT-STR                
051201         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
051301     END-IF                                                               
051401                                                                          
051501     SET  PLAA-IX              TO +2                                      
051601     MOVE WC-NDC-US-LA TO W-6005-IDDC                                     
051701     PERFORM IMS-GU-PLAA-PLAA11                                           
051801     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
051901         MOVE PLAA-6006-ADINLOMR TO W-PLAA-TAB-ADINLOMR(5 PLAA-IX)        
052001         MOVE PLAA-6006-KDINLUPF TO W-PLAA-TAB-KDINLUPF(5 PLAA-IX)        
052101         SET PLAA-IX UP BY +1                                             
052201         PERFORM IMS-GNP-PLAA-PLAA11                                      
052301     END-PERFORM                                                          
052401                                                                          
052501     IF PLAA-IX                   > MAX-PLAA-IX                           
052601         MOVE 'PLAA TABELL FULL FÖR DC=43 ' TO FELTEXT-STR                
052701         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
052801     END-IF                                                               
052901                                                                          
053001     SET  PLAA-IX              TO +2                                      
053101     MOVE WC-NDC-US-SE TO W-6005-IDDC                                     
053201     PERFORM IMS-GU-PLAA-PLAA11                                           
053301     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
053401         MOVE PLAA-6006-ADINLOMR TO W-PLAA-TAB-ADINLOMR(6 PLAA-IX)        
053501         MOVE PLAA-6006-KDINLUPF TO W-PLAA-TAB-KDINLUPF(6 PLAA-IX)        
053601         SET PLAA-IX UP BY +1                                             
053701         PERFORM IMS-GNP-PLAA-PLAA11                                      
053801     END-PERFORM                                                          
053901                                                                          
054001     IF PLAA-IX                   > MAX-PLAA-IX                           
054101         MOVE 'PLAA TABELL FULL FÖR DC=44 ' TO FELTEXT-STR                
054201         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
054301     END-IF                                                               
054401                                                                          
054501     SET  PLAA-IX              TO +2                                      
054601     MOVE WC-NDC-US-CH TO W-6005-IDDC                                     
054701     PERFORM IMS-GU-PLAA-PLAA11                                           
054801     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
054901         MOVE PLAA-6006-ADINLOMR TO W-PLAA-TAB-ADINLOMR(7 PLAA-IX)        
055001         MOVE PLAA-6006-KDINLUPF TO W-PLAA-TAB-KDINLUPF(7 PLAA-IX)        
055101         SET PLAA-IX UP BY +1                                             
055201         PERFORM IMS-GNP-PLAA-PLAA11                                      
055301     END-PERFORM                                                          
055401                                                                          
055501     IF PLAA-IX                   > MAX-PLAA-IX                           
055601         MOVE 'PLAA TABELL FULL FÖR DC=45 ' TO FELTEXT-STR                
055701         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
055801     END-IF                                                               
055901                                                                          
056001     SET  PLAA-IX              TO +2                                      
056101     MOVE WC-NDC-US-JA TO W-6005-IDDC                                     
056201     PERFORM IMS-GU-PLAA-PLAA11                                           
056301     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
056401         MOVE PLAA-6006-ADINLOMR TO W-PLAA-TAB-ADINLOMR(8 PLAA-IX)        
056501         MOVE PLAA-6006-KDINLUPF TO W-PLAA-TAB-KDINLUPF(8 PLAA-IX)        
056601         SET PLAA-IX UP BY +1                                             
056701         PERFORM IMS-GNP-PLAA-PLAA11                                      
056801     END-PERFORM                                                          
056901                                                                          
057001     IF PLAA-IX                   > MAX-PLAA-IX                           
057101         MOVE 'PLAA TABELL FULL FÖR DC=46 ' TO FELTEXT-STR                
057201         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
057301     END-IF                                                               
057401                                                                          
057501     SET  PLAA-IX              TO +2                                      
057601     MOVE WC-NDC-CA TO W-6005-IDDC                                        
057701     PERFORM IMS-GU-PLAA-PLAA11                                           
057801     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
057901         MOVE PLAA-6006-ADINLOMR TO W-PLAA-TAB-ADINLOMR(9 PLAA-IX)        
058001         MOVE PLAA-6006-KDINLUPF TO W-PLAA-TAB-KDINLUPF(9 PLAA-IX)        
058101         SET PLAA-IX UP BY +1                                             
058201         PERFORM IMS-GNP-PLAA-PLAA11                                      
058301     END-PERFORM                                                          
058401                                                                          
058501     IF PLAA-IX                   > MAX-PLAA-IX                           
058601         MOVE 'PLAA TABELL FULL FÖR DC=51 ' TO FELTEXT-STR                
058701         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
058801     END-IF                                                               
058901                                                                          
059001     SET  PLAA-IX              TO +2                                      
059101     MOVE WC-NDC-JP-61 TO W-6005-IDDC                                     
059201     PERFORM IMS-GU-PLAA-PLAA11                                           
059301     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
059401        MOVE PLAA-6006-ADINLOMR TO W-PLAA-TAB-ADINLOMR(10 PLAA-IX)        
059501        MOVE PLAA-6006-KDINLUPF TO W-PLAA-TAB-KDINLUPF(10 PLAA-IX)        
059601        SET PLAA-IX UP BY +1                                              
059701        PERFORM IMS-GNP-PLAA-PLAA11                                       
059801     END-PERFORM                                                          
059901                                                                          
060001     IF PLAA-IX                   > MAX-PLAA-IX                           
060101         MOVE 'PLAA TABELL FULL FÖR DC=61 ' TO FELTEXT-STR                
060201         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
060301     END-IF                                                               
060401                                                                          
060501     SET  PLAA-IX              TO +2                                      
060601     MOVE WC-NDC-AU TO W-6005-IDDC                                        
060701     PERFORM IMS-GU-PLAA-PLAA11                                           
060801     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
060901        MOVE PLAA-6006-ADINLOMR TO W-PLAA-TAB-ADINLOMR(11 PLAA-IX)        
061001        MOVE PLAA-6006-KDINLUPF TO W-PLAA-TAB-KDINLUPF(11 PLAA-IX)        
061101        SET PLAA-IX UP BY +1                                              
061201        PERFORM IMS-GNP-PLAA-PLAA11                                       
061301     END-PERFORM                                                          
061401                                                                          
061501     IF PLAA-IX                   > MAX-PLAA-IX                           
061601         MOVE 'PLAA TABELL FULL FÖR DC=62 ' TO FELTEXT-STR                
061701         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
061801     END-IF                                                               
061901                                                                          
062001     SET  PLAA-IX              TO +2                                      
062101     MOVE WC-NDC-IN TO W-6005-IDDC                                        
062201     PERFORM IMS-GU-PLAA-PLAA11                                           
062301     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
062401         MOVE PLAA-6006-ADINLOMR TO                                       
062501                                 W-PLAA-TAB-ADINLOMR(12 PLAA-IX)          
062601         MOVE PLAA-6006-KDINLUPF TO                                       
062701                                 W-PLAA-TAB-KDINLUPF(12 PLAA-IX)          
062801         SET PLAA-IX UP BY +1                                             
062901         PERFORM IMS-GNP-PLAA-PLAA11                                      
063001     END-PERFORM                                                          
063101                                                                          
063201     IF PLAA-IX                   > MAX-PLAA-IX                           
063301         MOVE 'PLAA TABELL FULL FÖR DC=67 ' TO FELTEXT-STR                
063401         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
063501     END-IF                                                               
063601                                                                          
063701     SET  PLAA-IX              TO +2                                      
063801     MOVE WC-NDC-CN-71 TO W-6005-IDDC                                     
063901     PERFORM IMS-GU-PLAA-PLAA11                                           
064001     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
064101         MOVE PLAA-6006-ADINLOMR TO                                       
064201                                 W-PLAA-TAB-ADINLOMR(13 PLAA-IX)          
064301         MOVE PLAA-6006-KDINLUPF TO                                       
064401                                 W-PLAA-TAB-KDINLUPF(13 PLAA-IX)          
064501         SET PLAA-IX UP BY +1                                             
064601         PERFORM IMS-GNP-PLAA-PLAA11                                      
064701     END-PERFORM                                                          
064801                                                                          
064901     IF PLAA-IX                   > MAX-PLAA-IX                           
065001         MOVE 'PLAA TABELL FULL FÖR DC=71 ' TO FELTEXT-STR                
065101         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
065201     END-IF                                                               
065301                                                                          
065401     SET  PLAA-IX              TO +2                                      
065501     MOVE WC-NDC-CN-72 TO W-6005-IDDC                                     
065601     PERFORM IMS-GU-PLAA-PLAA11                                           
065701     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
065801         MOVE PLAA-6006-ADINLOMR TO                                       
065901                                 W-PLAA-TAB-ADINLOMR(14 PLAA-IX)          
066001         MOVE PLAA-6006-KDINLUPF TO                                       
066101                                 W-PLAA-TAB-KDINLUPF(14 PLAA-IX)          
066201         SET PLAA-IX UP BY +1                                             
066301         PERFORM IMS-GNP-PLAA-PLAA11                                      
066401     END-PERFORM                                                          
066501                                                                          
066601     IF PLAA-IX                   > MAX-PLAA-IX                           
066701         MOVE 'PLAA TABELL FULL FÖR DC=72 ' TO FELTEXT-STR                
066801         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
066901     END-IF                                                               
067001                                                                          
067101     SET  PLAA-IX              TO +2                                      
067201     MOVE WC-NDC-CN-73 TO W-6005-IDDC                                     
067301     PERFORM IMS-GU-PLAA-PLAA11                                           
067401     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
067501         MOVE PLAA-6006-ADINLOMR TO                                       
067601                                 W-PLAA-TAB-ADINLOMR(15 PLAA-IX)          
067701         MOVE PLAA-6006-KDINLUPF TO                                       
067801                                 W-PLAA-TAB-KDINLUPF(15 PLAA-IX)          
067901         SET PLAA-IX UP BY +1                                             
068001         PERFORM IMS-GNP-PLAA-PLAA11                                      
068101     END-PERFORM                                                          
068201                                                                          
068301     IF PLAA-IX                   > MAX-PLAA-IX                           
068401         MOVE 'PLAA TABELL FULL FÖR DC=73 ' TO FELTEXT-STR                
068501         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
068601     END-IF                                                               
068701                                                                          
068801     SET  PLAA-IX              TO +2                                      
068901     MOVE WC-NDC-CN-74 TO W-6005-IDDC                                     
069001     PERFORM IMS-GU-PLAA-PLAA11                                           
069101     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
069201         MOVE PLAA-6006-ADINLOMR TO                                       
069301                                 W-PLAA-TAB-ADINLOMR(16 PLAA-IX)          
069401         MOVE PLAA-6006-KDINLUPF TO                                       
069501                                 W-PLAA-TAB-KDINLUPF(16 PLAA-IX)          
069601         SET PLAA-IX UP BY +1                                             
069701         PERFORM IMS-GNP-PLAA-PLAA11                                      
069801     END-PERFORM                                                          
069901                                                                          
070001     IF PLAA-IX                   > MAX-PLAA-IX                           
070101         MOVE 'PLAA TABELL FULL FÖR DC=74 ' TO FELTEXT-STR                
070201         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
070301     END-IF                                                               
070302                                                                          
070303     SET  PLAA-IX              TO +2                                      
070304     MOVE WC-NDC-KR TO W-6005-IDDC                                        
070305     PERFORM IMS-GU-PLAA-PLAA11                                           
070306     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
070307         MOVE PLAA-6006-ADINLOMR TO                                       
070308                                 W-PLAA-TAB-ADINLOMR(17 PLAA-IX)          
070309         MOVE PLAA-6006-KDINLUPF TO                                       
070310                                 W-PLAA-TAB-KDINLUPF(17 PLAA-IX)          
070311         SET PLAA-IX UP BY +1                                             
070320         PERFORM IMS-GNP-PLAA-PLAA11                                      
070330     END-PERFORM                                                          
070340                                                                          
070350     IF PLAA-IX                   > MAX-PLAA-IX                           
070360         MOVE 'PLAA TABELL FULL FÖR DC=65 ' TO FELTEXT-STR                
070370         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
070380     END-IF                                                               
070390                                                                          
070391     SET  PLAA-IX              TO +2                                      
070392     MOVE WC-NDC-AE TO W-6005-IDDC                                        
070393     PERFORM IMS-GU-PLAA-PLAA11                                           
070394     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
070395         MOVE PLAA-6006-ADINLOMR TO                                       
070396                                 W-PLAA-TAB-ADINLOMR(18 PLAA-IX)          
070397         MOVE PLAA-6006-KDINLUPF TO                                       
070398                                 W-PLAA-TAB-KDINLUPF(18 PLAA-IX)          
070399         SET PLAA-IX UP BY +1                                             
070400         PERFORM IMS-GNP-PLAA-PLAA11                                      
070401     END-PERFORM                                                          
070402                                                                          
070403     IF PLAA-IX                   > MAX-PLAA-IX                           
070404         MOVE 'PLAA TABELL FULL FÖR DC=87 ' TO FELTEXT-STR                
070405         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
070406     END-IF                                                               
070407                                                                          
070408     SET  PLAA-IX              TO +2                                      
070409     MOVE WC-NDC-TR TO W-6005-IDDC                                        
070410     PERFORM IMS-GU-PLAA-PLAA11                                           
070411     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
070412         MOVE PLAA-6006-ADINLOMR TO                                       
070413                                 W-PLAA-TAB-ADINLOMR(19 PLAA-IX)          
070414         MOVE PLAA-6006-KDINLUPF TO                                       
070415                                 W-PLAA-TAB-KDINLUPF(19 PLAA-IX)          
070416         SET PLAA-IX UP BY +1                                             
070417         PERFORM IMS-GNP-PLAA-PLAA11                                      
070418     END-PERFORM                                                          
070419                                                                          
070420     IF PLAA-IX                   > MAX-PLAA-IX                           
070421         MOVE 'PLAA TABELL FULL FÖR DC=86 ' TO FELTEXT-STR                
070422         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
070423     END-IF                                                               
070424                                                                          
070425     SET  PLAA-IX              TO +2                                      
070426     MOVE WC-NDC-MY TO W-6005-IDDC                                        
070427     PERFORM IMS-GU-PLAA-PLAA11                                           
070428     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
070429         MOVE PLAA-6006-ADINLOMR TO                                       
070430                                 W-PLAA-TAB-ADINLOMR(20 PLAA-IX)          
070431         MOVE PLAA-6006-KDINLUPF TO                                       
070432                                 W-PLAA-TAB-KDINLUPF(20 PLAA-IX)          
070433         SET PLAA-IX UP BY +1                                             
070434         PERFORM IMS-GNP-PLAA-PLAA11                                      
070435     END-PERFORM                                                          
070436                                                                          
070437     IF PLAA-IX                   > MAX-PLAA-IX                           
070438         MOVE 'PLAA TABELL FULL FÖR DC=66 ' TO FELTEXT-STR                
070439         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
070440     END-IF                                                               
070441                                                                          
070442     SET  PLAA-IX              TO +2                                      
070443     MOVE WC-NDC-TH TO W-6005-IDDC                                        
070444     PERFORM IMS-GU-PLAA-PLAA11                                           
070445     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
070446         MOVE PLAA-6006-ADINLOMR TO                                       
070447                                 W-PLAA-TAB-ADINLOMR(21 PLAA-IX)          
070448         MOVE PLAA-6006-KDINLUPF TO                                       
070449                                 W-PLAA-TAB-KDINLUPF(21 PLAA-IX)          
070450         SET PLAA-IX UP BY +1                                             
070451         PERFORM IMS-GNP-PLAA-PLAA11                                      
070452     END-PERFORM                                                          
070453                                                                          
070454     IF PLAA-IX                   > MAX-PLAA-IX                           
070455         MOVE 'PLAA TABELL FULL FÖR DC=63 ' TO FELTEXT-STR                
070456         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
070457     END-IF                                                               
070458                                                                          
070459     SET  PLAA-IX              TO +2                                      
070460     MOVE WC-NDC-TW TO W-6005-IDDC                                        
070461     PERFORM IMS-GU-PLAA-PLAA11                                           
070462     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
070463         MOVE PLAA-6006-ADINLOMR TO                                       
070464                                 W-PLAA-TAB-ADINLOMR(22 PLAA-IX)          
070465         MOVE PLAA-6006-KDINLUPF TO                                       
070466                                 W-PLAA-TAB-KDINLUPF(22 PLAA-IX)          
070467         SET PLAA-IX UP BY +1                                             
070468         PERFORM IMS-GNP-PLAA-PLAA11                                      
070469     END-PERFORM                                                          
070470                                                                          
070471     IF PLAA-IX                   > MAX-PLAA-IX                           
070472         MOVE 'PLAA TABELL FULL FÖR DC=64 ' TO FELTEXT-STR                
070473         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
070474     END-IF                                                               
070475                                                                          
070476     SET  PLAA-IX              TO +2                                      
070477     MOVE WC-NDC-US-DA TO W-6005-IDDC                                     
070478     PERFORM IMS-GU-PLAA-PLAA11                                           
070479     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
070480         MOVE PLAA-6006-ADINLOMR TO                                       
070481                                 W-PLAA-TAB-ADINLOMR(23 PLAA-IX)          
070482         MOVE PLAA-6006-KDINLUPF TO                                       
070483                                 W-PLAA-TAB-KDINLUPF(23 PLAA-IX)          
070484         SET PLAA-IX UP BY +1                                             
070485         PERFORM IMS-GNP-PLAA-PLAA11                                      
070486     END-PERFORM                                                          
070487                                                                          
070488     IF PLAA-IX                   > MAX-PLAA-IX                           
070489         MOVE 'PLAA TABELL FULL FÖR DC=47 ' TO FELTEXT-STR                
070490         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
070491     END-IF                                                               
070492                                                                          
070493     SET  PLAA-IX              TO +2                                      
070494     MOVE WC-NDC-BR    TO W-6005-IDDC                                     
070495     PERFORM IMS-GU-PLAA-PLAA11                                           
070496     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
070497         MOVE PLAA-6006-ADINLOMR TO                                       
070498                                 W-PLAA-TAB-ADINLOMR(24 PLAA-IX)          
070499         MOVE PLAA-6006-KDINLUPF TO                                       
070500                                 W-PLAA-TAB-KDINLUPF(24 PLAA-IX)          
070501         SET PLAA-IX UP BY +1                                             
070502         PERFORM IMS-GNP-PLAA-PLAA11                                      
070503     END-PERFORM                                                          
070504                                                                          
070505     IF PLAA-IX                   > MAX-PLAA-IX                           
070506         MOVE 'PLAA TABELL FULL FÖR DC=52 ' TO FELTEXT-STR                
070507         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
070508     END-IF                                                               
070509                                                                          
070510     SET  PLAA-IX              TO +2                                      
070511     MOVE WC-NDC-MX    TO W-6005-IDDC                                     
070512     PERFORM IMS-GU-PLAA-PLAA11                                           
070513     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
070514         MOVE PLAA-6006-ADINLOMR TO                                       
070515                                 W-PLAA-TAB-ADINLOMR(25 PLAA-IX)          
070516         MOVE PLAA-6006-KDINLUPF TO                                       
070517                                 W-PLAA-TAB-KDINLUPF(25 PLAA-IX)          
070518         SET PLAA-IX UP BY +1                                             
070519         PERFORM IMS-GNP-PLAA-PLAA11                                      
070520     END-PERFORM                                                          
070521                                                                          
070522     IF PLAA-IX                   > MAX-PLAA-IX                           
070523         MOVE 'PLAA TABELL FULL FÖR DC=53 ' TO FELTEXT-STR                
070524         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
070525     END-IF                                                               
070526                                                                          
070527     SET  PLAA-IX              TO +2                                      
070528     MOVE WC-NDC-TH-93 TO W-6005-IDDC                                     
070529     PERFORM IMS-GU-PLAA-PLAA11                                           
070530     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
070540         MOVE PLAA-6006-ADINLOMR TO                                       
070550                                 W-PLAA-TAB-ADINLOMR(26 PLAA-IX)          
070560         MOVE PLAA-6006-KDINLUPF TO                                       
070570                                 W-PLAA-TAB-KDINLUPF(26 PLAA-IX)          
070580         SET PLAA-IX UP BY +1                                             
070590         PERFORM IMS-GNP-PLAA-PLAA11                                      
070600     END-PERFORM                                                          
070700                                                                          
070800     IF PLAA-IX                   > MAX-PLAA-IX                           
070900         MOVE 'PLAA TABELL FULL FÖR DC=93 ' TO FELTEXT-STR                
071000         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
071001     END-IF                                                               
071002                                                                          
071003     SET  PLAA-IX              TO +2                                      
071004     MOVE WC-NDC-ZA    TO W-6005-IDDC                                     
071005     PERFORM IMS-GU-PLAA-PLAA11                                           
071006     PERFORM UNTIL SEGMENT-SAKNAS OR PLAA-IX > MAX-PLAA-IX                
071007         MOVE PLAA-6006-ADINLOMR TO                                       
071008                                 W-PLAA-TAB-ADINLOMR(27 PLAA-IX)          
071009         MOVE PLAA-6006-KDINLUPF TO                                       
071010                                 W-PLAA-TAB-KDINLUPF(27 PLAA-IX)          
071011         SET PLAA-IX UP BY +1                                             
071012         PERFORM IMS-GNP-PLAA-PLAA11                                      
071013     END-PERFORM                                                          
071014                                                                          
071015     IF PLAA-IX                   > MAX-PLAA-IX                           
071016         MOVE 'PLAA TABELL FULL FÖR DC=85 ' TO FELTEXT-STR                
071017         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
071018     END-IF                                                               
071019     .                                                                    
071020     EJECT                                                                
071021 C-BEHANDLA-INLEVERANSPOST  SECTION.                                      
071022                                                                          
071023     MOVE IN-IDDC TO WS-IDDC                                              
071024     IF CDC-SE                                                            
071030       MOVE +1            TO DC-IX                                        
071101     END-IF                                                               
071201     IF CDC-TR                                                            
071301       MOVE +2            TO DC-IX                                        
071401     END-IF                                                               
071501     IF NDC-US-RU                                                         
071601       MOVE +3            TO DC-IX                                        
071701     END-IF                                                               
071801     IF NDC-US-BAT                                                        
071901       MOVE +4            TO DC-IX                                        
072001     END-IF                                                               
072101     IF NDC-US-LA                                                         
072201       MOVE +5            TO DC-IX                                        
072301     END-IF                                                               
072401     IF NDC-US-SE                                                         
072501       MOVE +6            TO DC-IX                                        
072601     END-IF                                                               
072701     IF NDC-US-CH                                                         
072801       MOVE +7            TO DC-IX                                        
072901     END-IF                                                               
073001     IF NDC-US-JA                                                         
073101       MOVE +8            TO DC-IX                                        
073201     END-IF                                                               
073301     IF NDC-CA                                                            
073401       MOVE +9            TO DC-IX                                        
073501     END-IF                                                               
073601     IF NDC-JP-61                                                         
073701       MOVE +10           TO DC-IX                                        
073801     END-IF                                                               
073901     IF NDC-AU                                                            
074001       MOVE +11           TO DC-IX                                        
074101     END-IF                                                               
074201     IF NDC-IN                                                            
074301       MOVE +12           TO DC-IX                                        
074401     END-IF                                                               
074501     IF NDC-CN-71                                                         
074601       MOVE +13           TO DC-IX                                        
074701     END-IF                                                               
074801     IF NDC-CN-72                                                         
074901       MOVE +14           TO DC-IX                                        
075001     END-IF                                                               
075101     IF NDC-CN-73                                                         
075201       MOVE +15           TO DC-IX                                        
075301     END-IF                                                               
075401     IF NDC-CN-74                                                         
075501       MOVE +16           TO DC-IX                                        
075601     END-IF                                                               
075602     IF NDC-KR                                                            
075603       MOVE +17           TO DC-IX                                        
075604     END-IF                                                               
075605     IF NDC-AE                                                            
075606       MOVE +18           TO DC-IX                                        
075607     END-IF                                                               
075608     IF NDC-TR                                                            
075609       MOVE +19           TO DC-IX                                        
075610     END-IF                                                               
075620     IF NDC-MY                                                            
075630       MOVE +20           TO DC-IX                                        
075640     END-IF                                                               
075641     IF NDC-TH                                                            
075642       MOVE +21           TO DC-IX                                        
075643     END-IF                                                               
075644     IF NDC-TW                                                            
075645       MOVE +22           TO DC-IX                                        
075646     END-IF                                                               
075647     IF NDC-US-DA                                                         
075648       MOVE +23           TO DC-IX                                        
075649     END-IF                                                               
075650     IF NDC-BR                                                            
075651       MOVE +24           TO DC-IX                                        
075652     END-IF                                                               
075653     IF NDC-MX                                                            
075654       MOVE +25           TO DC-IX                                        
075655     END-IF                                                               
075656     IF NDC-TH-93                                                         
075657       MOVE +26           TO DC-IX                                        
075658     END-IF                                                               
075659     IF NDC-ZA                                                            
075660       MOVE +27           TO DC-IX                                        
075670     END-IF                                                               
075701                                                                          
075801     PERFORM CA-TA-FRAM-KDINLUPF                                          
075901                                                                          
076001     IF W-KDINLUPF             = SPACE                                    
076101         CONTINUE                                                         
076201     ELSE                                                                 
076301         PERFORM CB-KOLLA-OM-NY-KDINLUPF                                  
076401         PERFORM CC-BEHANDLA-ANTAL-PARTIER                                
076501         PERFORM CD-BEHANDLA-ANTAL-KOLLI                                  
076601         PERFORM CE-BEHANDLA-VAERDE                                       
076701         PERFORM CF-BEHANDLA-PRIO-VAERDE                                  
076801         PERFORM CG-BEHANDLA-ANTAL-RADER                                  
076901     END-IF                                                               
077001     .                                                                    
077101     EJECT                                                                
077201 CA-TA-FRAM-KDINLUPF         SECTION.                                     
077301                                                                          
077401     IF IN-ADINLOMR-NXT        =  SPACE                                   
077501         MOVE IN-ADINLOMR      TO W-ADINLOMR                              
077601     ELSE                                                                 
077701         MOVE IN-ADINLOMR-NXT  TO W-ADINLOMR                              
077801     END-IF                                                               
077901                                                                          
078001     IF  W-ADINLOMR             =  SPACE                                  
078101     AND IN-KDINLSTA            =  SPACE                                  
078201         MOVE 'R31 '           TO W-KDINLUPF                              
078301     ELSE                                                                 
078401       IF W-ADINLOMR            =  SPACE                                  
078501       AND IN-KDINLSTA          =  'AVI'                                  
078601         MOVE 'C2  '           TO W-KDINLUPF                              
078701       ELSE                                                               
078801         SET IDDC-IX           TO DC-IX                                   
078901         SET PLAA-IX           TO 1                                       
079001         SEARCH  W-PLAA-TAB                                               
079101         VARYING PLAA-IX                                                  
079201           AT END                                                         
079301             MOVE 'PLAA SAKNAS     ' TO FELTEXT-STR                       
079401             DISPLAY FELTEXT                                              
079501             DISPLAY 'PÅ INDEX ' DC-IX                                    
079601             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
079701           WHEN W-PLAA-TAB-ADINLOMR (IDDC-IX PLAA-IX) =                   
079801                                                      W-ADINLOMR          
079901             MOVE W-PLAA-TAB-KDINLUPF(IDDC-IX PLAA-IX) TO                 
080001                                                      W-KDINLUPF          
080101             CONTINUE                                                     
080201         END-SEARCH                                                       
080301       END-IF                                                             
080401     END-IF                                                               
080501     .                                                                    
080601     EJECT                                                                
080701 CB-KOLLA-OM-NY-KDINLUPF     SECTION.                                     
080801                                                                          
080901     SET SUM-IX   TO +1                                                   
081001     PERFORM UNTIL SUM-IX     > MAX-SUM-IX                                
081101             OR  W-SUM-KDINLUPF (DC-IX SUM-IX) = SPACE                    
081201             OR  W-SUM-KDINLUPF (DC-IX SUM-IX) = W-KDINLUPF               
081301         SET SUM-IX UP BY +1                                              
081401     END-PERFORM                                                          
081501                                                                          
081601     IF SUM-IX                 > MAX-SUM-IX                               
081701         MOVE 'SUM TAB FULL  ' TO FELTEXT-STR                             
081801         DISPLAY FELTEXT                                                  
081901         DISPLAY 'PÅ INDEX ' DC-IX                                        
082001         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
082101     ELSE                                                                 
082201         IF W-SUM-KDINLUPF (DC-IX SUM-IX) = SPACE                         
082301             MOVE W-KDINLUPF   TO W-SUM-KDINLUPF (DC-IX SUM-IX)           
082401         END-IF                                                           
082501     END-IF                                                               
082601     .                                                                    
082701     EJECT                                                                
082801 CC-BEHANDLA-ANTAL-PARTIER   SECTION.                                     
082901                                                                          
083001     IF IN-IDLOPNRM    = W-OLD-IDLOPNRM (DC-IX)                           
083101         PERFORM CCA-KOLLA-OM-NY-PARTI-KDINLUPF                           
083201         IF KDINLUPF-TRAEFF                                               
083301             CONTINUE                                                     
083401         ELSE                                                             
083501             ADD +1        TO W-SUM-KVART    (DC-IX SUM-IX)               
083601         END-IF                                                           
083701     ELSE                                                                 
083801         PERFORM CCB-INIT-PARTI-KDINLUPF-TAB                              
083901         ADD +1            TO W-SUM-KVART    (DC-IX SUM-IX)               
084001                              W-TOT-KVART    (DC-IX)                      
084101         MOVE IN-IDLOPNRM  TO W-OLD-IDLOPNRM (DC-IX)                      
084201     END-IF                                                               
084301                                                                          
084401     .                                                                    
084501     EJECT                                                                
084601 CCA-KOLLA-OM-NY-PARTI-KDINLUPF     SECTION.                              
084701                                                                          
084801     MOVE NEJ                  TO KDINLUPF-SW                             
084901     SET PK-IX                 TO +1                                      
085001     PERFORM UNTIL PK-IX       >  MAX-PK-IX                               
085101             OR    KDINLUPF-TRAEFF                                        
085201             OR    W-PARTI-KDINLUPF (DC-IX PK-IX) = SPACE                 
085301         IF W-KDINLUPF      =  W-PARTI-KDINLUPF (DC-IX PK-IX)             
085401             MOVE JA        TO KDINLUPF-SW                                
085501         END-IF                                                           
085601         SET PK-IX UP BY +1                                               
085701     END-PERFORM                                                          
085801                                                                          
085901     IF KDINLUPF-TRAEFF                                                   
086001         CONTINUE                                                         
086101     ELSE                                                                 
086201         IF PK-IX              > MAX-PK-IX                                
086301             MOVE 'PARTI TAB FULL  ' TO FELTEXT-STR                       
086401             DISPLAY FELTEXT                                              
086501             DISPLAY 'PÅ INDEX ' DC-IX                                    
086601             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
086701         ELSE                                                             
086801           MOVE W-KDINLUPF  TO W-PARTI-KDINLUPF (DC-IX PK-IX)             
086901         END-IF                                                           
087001     END-IF                                                               
087101     .                                                                    
087201     EJECT                                                                
087301 CCB-INIT-PARTI-KDINLUPF-TAB SECTION.                                     
087401                                                                          
087501     SET PK-IX                 TO +1                                      
087601     PERFORM UNTIL PK-IX       >  MAX-PK-IX                               
087701         MOVE SPACE            TO W-PARTI-KDINLUPF (DC-IX PK-IX)          
087801         SET PK-IX UP BY +1                                               
087901     END-PERFORM                                                          
088001                                                                          
088101     MOVE W-KDINLUPF           TO W-PARTI-KDINLUPF (DC-IX  1)             
088201     .                                                                    
088301     EJECT                                                                
088401 CD-BEHANDLA-ANTAL-KOLLI     SECTION.                                     
088501                                                                          
088601     IF  IN-IDOKOLLI            >  ZERO                                   
088701     AND IN-FLDIVKLI            =  NEJ                                    
088801         ADD +1                TO W-SUM-KVKOLLI (DC-IX SUM-IX)            
088901                                  W-TOT-KVKOLLI (DC-IX)                   
089001         IF IN-RAD-KDINLPRIO < +31                                        
089101           ADD +1          TO W-SUM-KVKOLLI-PRIO (DC-IX SUM-IX)           
089201                              W-TOT-KVKOLLI-PRIO (DC-IX)                  
089301         END-IF                                                           
089401     END-IF                                                               
089501     .                                                                    
089601     EJECT                                                                
089701 CE-BEHANDLA-VAERDE          SECTION.                                     
089801                                                                          
089901     COMPUTE W-SUM-SUBEL (DC-IX SUM-IX)                                   
090001           = W-SUM-SUBEL (DC-IX SUM-IX)                                   
090101           + (IN-KVINLART * IN-PRARTSTD)                                  
090201                                                                          
090301     COMPUTE W-TOT-SUBEL (DC-IX)                                          
090401           = W-TOT-SUBEL (DC-IX)                                          
090501           + (IN-KVINLART * IN-PRARTSTD)                                  
090601     .                                                                    
090701     EJECT                                                                
090801 CF-BEHANDLA-PRIO-VAERDE     SECTION.                                     
090901                                                                          
091001     IF IN-RAD-KDINLPRIO         < +31                                    
091101         COMPUTE W-SUM-SUBEL-PRIO (DC-IX SUM-IX)                          
091201               = W-SUM-SUBEL-PRIO (DC-IX SUM-IX)                          
091301               + (IN-KVINLART * IN-PRARTSTD)                              
091401                                                                          
091501         COMPUTE W-TOT-SUBEL-PRIO (DC-IX)                                 
091601               = W-TOT-SUBEL-PRIO (DC-IX)                                 
091701               + (IN-KVINLART * IN-PRARTSTD)                              
091801     END-IF                                                               
091901     .                                                                    
092001     EJECT                                                                
092101 CG-BEHANDLA-ANTAL-RADER     SECTION.                                     
092201                                                                          
092301     ADD +1                TO W-SUM-KVRADER (DC-IX SUM-IX)                
092401                              W-TOT-KVRADER (DC-IX)                       
092501     IF IN-RAD-KDINLPRIO < +31                                            
092601       ADD +1          TO W-SUM-KVRADER-PRIO (DC-IX SUM-IX)               
092701                          W-TOT-KVRADER-PRIO (DC-IX)                      
092801     END-IF                                                               
092901     .                                                                    
093001     EJECT                                                                
093101 D-TOEM-KDINLUPF-TAB        SECTION.                                      
093201                                                                          
093301     MOVE 'RAD'                    TO UT-IDPTYP                           
093401                                                                          
093501     MOVE  +1    TO  DC-IX                                                
093601     SET SUM-IX  TO   +1                                                  
093701                                                                          
093801     PERFORM UNTIL DC-IX  >  MAX-PK-IX                                    
093901       IF DC-IX = +1                                                      
094001         MOVE WC-CDC-SE      TO UT-IDDC                                   
094101       END-IF                                                             
094201       IF DC-IX = +2                                                      
094301         MOVE WC-CDC-TR      TO UT-IDDC                                   
094401       END-IF                                                             
094501       IF DC-IX = +3                                                      
094601         MOVE WC-NDC-US-RU   TO UT-IDDC                                   
094701       END-IF                                                             
094801       IF DC-IX = +4                                                      
094901         MOVE WC-NDC-US-BAT  TO UT-IDDC                                   
095001       END-IF                                                             
095101       IF DC-IX = +5                                                      
095201         MOVE WC-NDC-US-LA   TO UT-IDDC                                   
095301       END-IF                                                             
095401       IF DC-IX = +6                                                      
095501         MOVE WC-NDC-US-SE   TO UT-IDDC                                   
095601       END-IF                                                             
095701       IF DC-IX = +7                                                      
095801         MOVE WC-NDC-US-CH   TO UT-IDDC                                   
095901       END-IF                                                             
096001       IF DC-IX = +8                                                      
096101         MOVE WC-NDC-US-JA   TO UT-IDDC                                   
096201       END-IF                                                             
096301       IF DC-IX = +9                                                      
096401         MOVE WC-NDC-CA      TO UT-IDDC                                   
096501       END-IF                                                             
096601       IF DC-IX = +10                                                     
096701         MOVE WC-NDC-JP-61   TO UT-IDDC                                   
096801       END-IF                                                             
096901       IF DC-IX = +11                                                     
097001         MOVE WC-NDC-AU      TO UT-IDDC                                   
097101       END-IF                                                             
097201       IF DC-IX = +12                                                     
097301         MOVE WC-NDC-IN      TO UT-IDDC                                   
097401       END-IF                                                             
097501       IF DC-IX = +13                                                     
097601         MOVE WC-NDC-CN-71   TO UT-IDDC                                   
097701       END-IF                                                             
097801       IF DC-IX = +14                                                     
097901         MOVE WC-NDC-CN-72   TO UT-IDDC                                   
098001       END-IF                                                             
098101       IF DC-IX = +15                                                     
098201         MOVE WC-NDC-CN-73   TO UT-IDDC                                   
098301       END-IF                                                             
098401       IF DC-IX = +16                                                     
098501         MOVE WC-NDC-CN-74   TO UT-IDDC                                   
098601       END-IF                                                             
098602       IF DC-IX = +17                                                     
098603         MOVE WC-NDC-KR      TO UT-IDDC                                   
098604       END-IF                                                             
098605       IF DC-IX = +18                                                     
098606         MOVE WC-NDC-AE      TO UT-IDDC                                   
098607       END-IF                                                             
098608       IF DC-IX = +19                                                     
098609         MOVE WC-NDC-TR      TO UT-IDDC                                   
098610       END-IF                                                             
098620       IF DC-IX = +20                                                     
098630         MOVE WC-NDC-MY      TO UT-IDDC                                   
098640       END-IF                                                             
098641       IF DC-IX = +21                                                     
098642         MOVE WC-NDC-TH      TO UT-IDDC                                   
098643       END-IF                                                             
098644       IF DC-IX = +22                                                     
098645         MOVE WC-NDC-TW      TO UT-IDDC                                   
098646       END-IF                                                             
098647       IF DC-IX = +23                                                     
098648         MOVE WC-NDC-US-DA   TO UT-IDDC                                   
098649       END-IF                                                             
098650       IF DC-IX = +24                                                     
098651         MOVE WC-NDC-BR      TO UT-IDDC                                   
098652       END-IF                                                             
098653       IF DC-IX = +25                                                     
098654         MOVE WC-NDC-MX      TO UT-IDDC                                   
098655       END-IF                                                             
098656       IF DC-IX = +26                                                     
098657         MOVE WC-NDC-TH-93   TO UT-IDDC                                   
098658       END-IF                                                             
098659       IF DC-IX = +27                                                     
098660         MOVE WC-NDC-ZA      TO UT-IDDC                                   
098670       END-IF                                                             
098701       PERFORM UNTIL SUM-IX >  MAX-SUM-IX                                 
098801                OR   W-SUM-KDINLUPF(DC-IX SUM-IX) =  SPACE                
098901         MOVE W-SUM-KDINLUPF  (DC-IX SUM-IX)   TO UT-KDINLUPF             
099001         MOVE W-SUM-KVART     (DC-IX SUM-IX)   TO UT-KVART                
099101         MOVE W-SUM-KVRADER   (DC-IX SUM-IX)   TO UT-KVRADER              
099201         MOVE W-SUM-KVRADER-PRIO(DC-IX SUM-IX) TO UT-KVRADER-PRIO         
099301         MOVE W-SUM-KVKOLLI   (DC-IX SUM-IX)   TO UT-KVKOLLI              
099401         MOVE W-SUM-KVKOLLI-PRIO(DC-IX SUM-IX) TO UT-KVKOLLI-PRIO         
099501         MOVE W-SUM-SUBEL     (DC-IX SUM-IX)   TO UT-SUBEL                
099601         MOVE W-SUM-SUBEL-PRIO(DC-IX SUM-IX)   TO UT-SUBEL-PRIO           
099701                                                                          
099801         PERFORM S11-SKRIV-W61150                                         
099901                                                                          
100001         SET SUM-IX UP BY +1                                              
100101       END-PERFORM                                                        
100201       SET SUM-IX  TO   +1                                                
100301       ADD +1 TO DC-IX                                                    
100401     END-PERFORM                                                          
100501     .                                                                    
100601     EJECT                                                                
100701 E-SKRIV-TOTAL-POST         SECTION.                                      
100801                                                                          
100901     MOVE 'TOT'               TO UT-IDPTYP                                
101001                                                                          
101101     MOVE  +1    TO  DC-IX                                                
101201                                                                          
101301     PERFORM UNTIL DC-IX  >  MAX-PK-IX                                    
101401       IF DC-IX = +1                                                      
101501         MOVE WC-CDC-SE     TO UT-IDDC                                    
101601       END-IF                                                             
101701       IF DC-IX = +2                                                      
101801         MOVE WC-CDC-TR     TO UT-IDDC                                    
101901       END-IF                                                             
102001       IF DC-IX = +3                                                      
102101         MOVE WC-NDC-US-RU  TO UT-IDDC                                    
102201       END-IF                                                             
102301       IF DC-IX = +4                                                      
102401         MOVE WC-NDC-US-BAT TO UT-IDDC                                    
102501       END-IF                                                             
102601       IF DC-IX = +5                                                      
102701         MOVE WC-NDC-US-LA  TO UT-IDDC                                    
102801       END-IF                                                             
102901       IF DC-IX = +6                                                      
103001         MOVE WC-NDC-US-SE  TO UT-IDDC                                    
103101       END-IF                                                             
103201       IF DC-IX = +7                                                      
103301         MOVE WC-NDC-US-CH  TO UT-IDDC                                    
103401       END-IF                                                             
103501       IF DC-IX = +8                                                      
103601         MOVE WC-NDC-US-JA  TO UT-IDDC                                    
103701       END-IF                                                             
103801       IF DC-IX = +9                                                      
103901         MOVE WC-NDC-CA     TO UT-IDDC                                    
104001       END-IF                                                             
104101       IF DC-IX = +10                                                     
104201         MOVE WC-NDC-JP-61     TO UT-IDDC                                 
104301       END-IF                                                             
104401       IF DC-IX = +11                                                     
104501         MOVE WC-NDC-AU        TO UT-IDDC                                 
104601       END-IF                                                             
104701       IF DC-IX = +12                                                     
104801         MOVE WC-NDC-IN        TO UT-IDDC                                 
104901       END-IF                                                             
105001       IF DC-IX = +13                                                     
105101         MOVE WC-NDC-CN-71     TO UT-IDDC                                 
105201       END-IF                                                             
105301       IF DC-IX = +14                                                     
105401         MOVE WC-NDC-CN-72     TO UT-IDDC                                 
105501       END-IF                                                             
105601       IF DC-IX = +15                                                     
105701         MOVE WC-NDC-CN-73     TO UT-IDDC                                 
105801       END-IF                                                             
105901       IF DC-IX = +16                                                     
106001         MOVE WC-NDC-CN-74     TO UT-IDDC                                 
106101       END-IF                                                             
106102       IF DC-IX = +17                                                     
106103         MOVE WC-NDC-KR        TO UT-IDDC                                 
106104       END-IF                                                             
106105       IF DC-IX = +18                                                     
106106         MOVE WC-NDC-AE        TO UT-IDDC                                 
106107       END-IF                                                             
106108       IF DC-IX = +19                                                     
106109         MOVE WC-NDC-TR        TO UT-IDDC                                 
106110       END-IF                                                             
106120       IF DC-IX = +20                                                     
106130         MOVE WC-NDC-MY        TO UT-IDDC                                 
106140       END-IF                                                             
106141       IF DC-IX = +21                                                     
106142         MOVE WC-NDC-TH        TO UT-IDDC                                 
106143       END-IF                                                             
106144       IF DC-IX = +22                                                     
106145         MOVE WC-NDC-TW        TO UT-IDDC                                 
106146       END-IF                                                             
106147       IF DC-IX = +23                                                     
106148         MOVE WC-NDC-US-DA     TO UT-IDDC                                 
106149       END-IF                                                             
106150       IF DC-IX = +24                                                     
106151         MOVE WC-NDC-BR        TO UT-IDDC                                 
106152       END-IF                                                             
106153       IF DC-IX = +25                                                     
106154         MOVE WC-NDC-MX        TO UT-IDDC                                 
106155       END-IF                                                             
106156       IF DC-IX = +26                                                     
106157         MOVE WC-NDC-TH-93     TO UT-IDDC                                 
106158       END-IF                                                             
106159       IF DC-IX = +27                                                     
106160         MOVE WC-NDC-ZA        TO UT-IDDC                                 
106170       END-IF                                                             
106201       MOVE SPACE                      TO UT-KDINLUPF                     
106301       MOVE W-TOT-KVART        (DC-IX) TO UT-KVART                        
106401       MOVE W-TOT-KVRADER      (DC-IX) TO UT-KVRADER                      
106501       MOVE W-TOT-KVRADER-PRIO (DC-IX) TO UT-KVRADER-PRIO                 
106601       MOVE W-TOT-KVKOLLI      (DC-IX) TO UT-KVKOLLI                      
106701       MOVE W-TOT-KVKOLLI-PRIO (DC-IX) TO UT-KVKOLLI-PRIO                 
106801       MOVE W-TOT-SUBEL        (DC-IX) TO UT-SUBEL                        
106901       MOVE W-TOT-SUBEL-PRIO   (DC-IX) TO UT-SUBEL-PRIO                   
107001                                                                          
107101       PERFORM S11-SKRIV-W61150                                           
107201                                                                          
107301       ADD +1 TO DC-IX                                                    
107401     END-PERFORM                                                          
107501     .                                                                    
107601     EJECT                                                                
107701 Z-FINIT SECTION.                                                         
107801     CLOSE W61110                                                         
107901           W61150                                                         
108001     SKIP2                                                                
108101     MOVE 'S' TO POSTSUM-OPKOD                                            
108201     CALL POSTSUM USING POSTSUM-PARM                                      
108301     .                                                                    
108401     EJECT                                                                
108501 S01-LAES-W61110  SECTION.                                                
108601     SKIP2                                                                
108701     READ W61110 INTO IN-AREA                                             
108801     AT END                                                               
108901        SET END-OF-W61110 TO TRUE                                         
109001                                                                          
109101     NOT AT END                                                           
109201        MOVE 'W61110' TO POSTSUM-FDNAMN                                   
109301        MOVE 'W61150D1' TO POSTSUM-DDNAMN2                                
109401        CALL POSTSUM USING POSTSUM-PARM                                   
109501     END-READ                                                             
109601     .                                                                    
109701     EJECT                                                                
109801 S11-SKRIV-W61150 SECTION.                                                
109901     SKIP2                                                                
110001     WRITE UT-POST FROM UT-AREA                                           
110101                                                                          
110201     MOVE UT-IDPTYP            TO POSTSUM-TRANSTYP                        
110301     MOVE 'W61150'             TO POSTSUM-FDNAMN                          
110401     MOVE 'W61150D2'           TO POSTSUM-DDNAMN2                         
110501     CALL POSTSUM USING POSTSUM-PARM                                      
110601     .                                                                    
110701     EJECT                                                                
110801* --- IMS SEKTIONER ---                                                   
110901     SKIP3                                                                
111001 IMS-GU-PLAA-PLAA11 SECTION.                                              
111101     STRING 'W6PLAA01*P(W6GXKEY  =' W-W6GXKEY-6005-X ')'                  
111201          DELIMITED BY SIZE INTO SSA1                                     
111301     MOVE 'W6PLAA11'          TO SSA2                                     
111401     MOVE '  GE' TO GODK-STATUSKODER                                      
111501     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
111601     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
111701     PERFORM IMS-STATUSKONTROLL                                           
111801     .                                                                    
111901     EJECT                                                                
112001 IMS-GNP-PLAA-PLAA11 SECTION.                                             
112101     MOVE 'W6PLAA11'          TO SSA1                                     
112201     MOVE '  GE' TO GODK-STATUSKODER                                      
112301     CALL CBLTDLI USING GNP PLAA-PCB DLI-IO-AREA SSA1                     
112401     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
112501     PERFORM IMS-STATUSKONTROLL                                           
112601     .                                                                    
112701     SKIP3                                                                
112801 IMS-STATUSKONTROLL SECTION.                                              
112901     SKIP2                                                                
113001     SET STATUS-IX TO 1                                                   
113101     SEARCH GODK-STATUS                                                   
113201       AT END                                                             
113301         MOVE 'FEL VID DL1 CALL' TO FELTEXT-STR                           
113401         DISPLAY FELTEXT                                                  
113501         CALL FELLOG                                                      
113601       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
113701         CONTINUE                                                         
114001     END-SEARCH                                                           
120000     .                                                                    
