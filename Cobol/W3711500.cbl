001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W3711500.                                                
001200 AUTHOR.         MARKUS ASPFJÄLL.                                         
001300 DATE-WRITTEN.   99/12/20.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        LÄSER FIL W37114 OCH KOMPLETTERAR DEN MED INFO IFRÅN WDK6        
001800*        SKAPAR SEDAN EN NY UTFIL SOM SORTERAS I PROCEDUREN               
001900*        PÅ DIST OCH KONTO                                                
002000*                                                                         
002110*        PROGRAMMET LÄSER      WDK6                                       
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- INFIL MED INFO IFRÅN WDA7                                  
003403     SELECT W37114                     ASSIGN TO W37115D1.                
003404     SKIP2                                                                
003405*          --- UTFIL                                                      
003410     SELECT W37115                     ASSIGN TO W37115D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W37114                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004006*01  -COPY W37109      -L.                                                
004007     SKIP3                                                                
004008 FD  W37115                                                               
004009     RECORDING       F                                                    
004010     BLOCK CONTAINS  0.                                                   
004011                                                                          
004020*01  POST -COPY W37109 -PRE  UT-  -L.                                     
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W3711500'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004801 01  WS-IDARTNR                  PIC 9(9) COMP-3 VALUE ZERO.              
004802                                                                          
004803 77  W37114-EOF-SW               PIC X       VALUE 'N'.                   
004810     88  END-OF-W37114                       VALUE 'J'.                   
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  BYTESENHETER                PIC X(24) VALUE                          
007403                                 'BYTES-AREA-START'.                      
007404*01  -COPY WWBYT16                                                        
007405     EJECT                                                                
007406 01  IN-AREA-START               PIC X(24)   VALUE                        
007407                                 'IN-AREA-START  '.                       
007408     SKIP2                                                                
007409                                                                          
007410*01  AREA -COPY W37109     -PRE IN-                                       
007411     EJECT                                                                
007412 01  UT-AREA-START               PIC X(24)   VALUE                        
007413                                 'UT-AREA-START  '.                       
007414     SKIP2                                                                
007415                                                                          
007420*01  AREA -COPY W37109     -PRE UT-                                       
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-IDARTNR-KVV-FKN-X.                                             
008202         05  W-IDARTNR-KVV-FKN   PIC S9(9)   VALUE ZERO COMP-3.           
008203     03  W-IDARTNR-X.                                                     
008204         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008205     03  W-IDARTNR-WDD3-X.                                                
008206         05  W-IDARTNR-WDD3      PIC S9(9)   VALUE ZERO COMP-3.           
008207     03  W-KDSEGKEY-X.                                                    
008208         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008209     03  W-IDSKYLT-X.                                                     
008210         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
008220                                                                          
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
010102 01  DLI-IO-WDK601.                                                       
010103*    03  -COPY WDK601                                                     
010104     EJECT                                                                
010105 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010106 01  DLI-IO-WDK611.                                                       
010107*    03  -COPY WDK611                                                     
010108     EJECT                                                                
010109 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK628'.                      
010110 01  DLI-IO-WDK628.                                                       
010120*    03  -COPY WDK628                                                     
010400     EJECT                                                                
010410 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
010420 01  DLI-IO-WDD311.                                                       
010430*    03  -COPY WDD311                                                     
010440     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010701                                                                          
010702*01  -COPY W0008  -PRE WDK6-                                              
010710     05  FILLER                  PIC X.                                   
010720*01  -COPY W0008  -PRE WDD3-                                              
010730     05  FILLER                  PIC X.                                   
010800     EJECT                                                                
010901 PROCEDURE DIVISION  USING WDK6-PCB WDD3-PCB.                             
010902 MAIN SECTION.                                                            
010910***  ENTRY 'DLITCBL' USING WDK6-PCB WDD3-PCB.                             
011200                                                                          
011300     PERFORM A-INIT                                                       
011400                                                                          
011510     PERFORM S01-LAES-W37114                                              
011600     PERFORM UNTIL END-OF-W37114                                          
011700       IF IN-IDPTYP = 'ADJ'                                               
011701                                                                          
011702         PERFORM C-SKRIV-ADJ-POST                                         
011703         PERFORM S11-SKRIV-W37115                                         
011704       ELSE                                                               
011710         MOVE IN-IDARTNR              TO WS-IDARTNR                       
011711                                         BYT16-IDARTNR                    
011712                                         W-IDARTNR-KVV-FKN                
011713                                         W-IDARTNR-WDD3                   
011714         IF BYT16-BYTES                                                   
011715           ADD 6000                   TO WS-IDARTNR                       
011716         ELSE                                                             
011717           ADD 1000                   TO WS-IDARTNR                       
011718         END-IF                                                           
011730         MOVE WS-IDARTNR              TO W-IDARTNR                        
011750         PERFORM IMS-GET-WDK601                                           
011760         IF SEGMENT-FINNS                                                 
011770           PERFORM IMS-GET-WDK611                                         
011790           IF SEGMENT-FINNS                                               
011795             IF CLAG-KVPOINT > 0                                          
011799               PERFORM B-FLYTTA-DATA                                      
011800               PERFORM S11-SKRIV-W37115                                   
011801             END-IF                                                       
011802           END-IF                                                         
011803         ELSE                                                             
011804           MOVE ZERO TO W-IDARTNR                                         
011805         END-IF                                                           
011820       END-IF                                                             
011830                                                                          
012310       PERFORM S01-LAES-W37114                                            
012400     END-PERFORM                                                          
012500                                                                          
012700     PERFORM Z-FINIT                                                      
012800                                                                          
012900     MOVE ZERO TO RETURN-CODE                                             
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013300 A-INIT SECTION.                                                          
013401                                                                          
013410     OPEN INPUT  W37114                                                   
013501                                                                          
013510     OPEN OUTPUT W37115                                                   
013600                                                                          
013700     ACCEPT DAGENS-DATUM  FROM DATE                                       
013810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014110 B-FLYTTA-DATA SECTION.                                                   
014120     MOVE IN-AREA            TO UT-AREA                                   
014122     MOVE CLAG-KVPOINT       TO UT-KVPOINT                                
014123     MOVE CLAG-KDEXCHA       TO UT-KDEXCHA                                
014124     COMPUTE UT-SUPOINT = UT-KVANTAL * UT-KVPOINT                         
014126*    IF IN-IDPTYP = 'KRE'                                                 
014127*      COMPUTE UT-SUPOINT = UT-SUPOINT * -1                               
014128*    END-IF                                                               
014129                                                                          
014130     MOVE 'GB '              TO W-IDSKYLT                                 
014131     PERFORM IMS-GET-WDD311                                               
014134     IF SEGMENT-FINNS                                                     
014135       MOVE TEXT-BEART       TO UT-BEART-ENG                              
014136     END-IF                                                               
014137                                                                          
014138     PERFORM IMS-GET-WDK601-KVV-FKN                                       
014139     IF SEGMENT-FINNS                                                     
014140       MOVE ART-IDFKNGRP     TO UT-IDFKNGRP                               
014141                                                                          
014142       PERFORM IMS-GET-WDK611-KVV-FKN                                     
014143       PERFORM IMS-GET-WDK628-KVV-FKN                                     
014144       IF SEGMENT-FINNS                                                   
014147         MOVE BYT-KVVECKOR   TO UT-KVVECKOR                               
014148       ELSE                                                               
014149         MOVE ZERO           TO UT-KVVECKOR                               
014150       END-IF                                                             
014151     ELSE                                                                 
014152                                                                          
014153       MOVE ART-IDFKNGRP     TO UT-IDFKNGRP                               
014154       MOVE ZERO             TO UT-KVVECKOR                               
014155                                                                          
014156       MOVE WS-IDARTNR       TO W-IDARTNR-WDD3                            
014157       PERFORM IMS-GET-WDD311                                             
014158       IF SEGMENT-FINNS                                                   
014159         MOVE TEXT-BEART     TO UT-BEART-ENG                              
014160       END-IF                                                             
014161     END-IF                                                               
014162     .                                                                    
014163     EJECT                                                                
014164 C-SKRIV-ADJ-POST SECTION.                                                
014165                                                                          
014166     MOVE IN-AREA            TO UT-AREA                                   
014167     MOVE IN-KVPOINT         TO UT-SUPOINT                                
014168     MOVE +1                 TO UT-KVANTAL                                
014169                                                                          
014170     .                                                                    
014180     EJECT                                                                
014200 Z-FINIT SECTION.                                                         
014301     CLOSE W37114                                                         
014310           W37115                                                         
014401     SKIP2                                                                
014402     MOVE 'S' TO POSTSUM-OPKOD                                            
014410     CALL POSTSUM USING POSTSUM-PARM                                      
014500     .                                                                    
014601     EJECT                                                                
014602 S01-LAES-W37114  SECTION.                                                
014603     READ W37114 INTO IN-AREA                                             
014604     AT END                                                               
014605        MOVE HIGH-VALUE TO IN-AREA                                        
014606        SET END-OF-W37114 TO TRUE                                         
014607                                                                          
014608     NOT AT END                                                           
014609        MOVE 'W37114' TO POSTSUM-FDNAMN                                   
014610        MOVE 'W37115D1' TO POSTSUM-DDNAMN2                                
014611*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
014612*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
014613        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
014614        CALL POSTSUM USING POSTSUM-PARM                                   
014615     END-READ                                                             
014620     .                                                                    
014701     EJECT                                                                
014702 S11-SKRIV-W37115 SECTION.                                                
014703                                                                          
014704     WRITE UT-POST FROM UT-AREA                                           
014705                                                                          
014706     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
014707     MOVE 'W37115' TO POSTSUM-FDNAMN                                      
014708     MOVE 'W37115D2' TO POSTSUM-DDNAMN2                                   
014709     CALL POSTSUM USING POSTSUM-PARM                                      
014710     .                                                                    
014900     EJECT                                                                
014910 IMS-GET-WDD311 SECTION.                                                  
014920                                                                          
014930     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-WDD3-X ')'                    
014940          DELIMITED BY SIZE INTO SSA1                                     
014950     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
014960          DELIMITED BY SIZE INTO SSA2                                     
014970     MOVE '  GE' TO GODK-STATUSKODER                                      
014980     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311   SSA1 SSA2             
014990     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
014991     PERFORM IMS-STATUSKONTROLL                                           
014992     .                                                                    
014993     EJECT                                                                
015000 S99-ABEND SECTION.                                                       
015100                                                                          
015201     SKIP2                                                                
015202     MOVE 'S' TO POSTSUM-OPKOD                                            
015210     CALL POSTSUM USING POSTSUM-PARM                                      
015300     CALL ABEND USING RKOD-ABEND                                          
015400     .                                                                    
015500     EJECT                                                                
015600* --- IMS SEKTIONER ---                                                   
015700                                                                          
015801     EJECT                                                                
015802 IMS-GET-WDK601 SECTION.                                                  
015803                                                                          
015804     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
015805          DELIMITED BY SIZE INTO SSA1                                     
015806     MOVE '  GE' TO GODK-STATUSKODER                                      
015807     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
015808     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
015809     PERFORM IMS-STATUSKONTROLL                                           
015810     .                                                                    
015811     EJECT                                                                
015812 IMS-GET-WDK601-KVV-FKN SECTION.                                          
015813                                                                          
015814     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-KVV-FKN-X ')'                 
015815          DELIMITED BY SIZE INTO SSA1                                     
015816     MOVE '  GE' TO GODK-STATUSKODER                                      
015817     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
015818     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
015819     PERFORM IMS-STATUSKONTROLL                                           
015820     .                                                                    
015821     EJECT                                                                
015822 IMS-GET-WDK611 SECTION.                                                  
015823                                                                          
015824     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
015825          DELIMITED BY SIZE INTO SSA1                                     
015826     MOVE '  GE' TO GODK-STATUSKODER                                      
015827     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
015828     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
015829     PERFORM IMS-STATUSKONTROLL                                           
015830     .                                                                    
015831     EJECT                                                                
015832 IMS-GET-WDK611-KVV-FKN SECTION.                                          
015833                                                                          
015834     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
015835          DELIMITED BY SIZE INTO SSA1                                     
015836     MOVE '  GE' TO GODK-STATUSKODER                                      
015837     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
015838     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
015839     PERFORM IMS-STATUSKONTROLL                                           
015840     .                                                                    
015841     EJECT                                                                
015910 IMS-GET-WDK628-KVV-FKN SECTION.                                          
015920                                                                          
015930     STRING 'WDK628  (IDARTNR  =' W-IDARTNR-KVV-FKN-X ')'                 
015940          DELIMITED BY SIZE INTO SSA1                                     
015950     MOVE '  GE' TO GODK-STATUSKODER                                      
015960     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK628 SSA1                   
015970     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
015980     PERFORM IMS-STATUSKONTROLL                                           
015990     .                                                                    
015991     EJECT                                                                
016000 IMS-STATUSKONTROLL SECTION.                                              
016100                                                                          
016200     SET STATUS-IX TO 1                                                   
016300     SEARCH GODK-STATUS                                                   
016400       AT END                                                             
016500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016600           DELIMITED BY SIZE INTO FELTEXT                                 
016700         DISPLAY FELTEXT                                                  
016800         CALL FELLOG                                                      
016900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017000         CONTINUE                                                         
017100     END-SEARCH                                                           
017200     .                                                                    
