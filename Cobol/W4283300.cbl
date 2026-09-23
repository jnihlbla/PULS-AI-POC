001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4283300.                                                
001300 AUTHOR.         SUSANNE OLSSON.                                          
001400 DATE-WRITTEN.   01/03/06.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        PROGRAMMET SUMMERAR STANDARTVÄRDET PÅ RETURARTIKLAR              
001900*        FRÅN NDC-USA/CAN , DISTRIKT 8111 OCH 8211.                       
002000*                                                                         
002100*        INFILER: W428.W428D1.W42831                                      
002101*                 MOMENTANVÄRDET PÅ RETURER I STATUS 4, 5 OCH 6           
002102*                 W428.W428D1.W42832                                      
002103*                 DAGENS RETURER SOM FÅTT STATUS 7                        
002104*                                                                         
002105*        PROGRAMMET LÄSER      WDA2                                       
002110*        PROGRAMMET LÄSER      WDK6                                       
002200*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- DAGENS RETURER KOD 54/94 I STATUS 4, 5, 6 OCH 7            
003403     SELECT INFIL                      ASSIGN TO W42833D1.                
003404     SKIP2                                                                
003405*          --- KOMPLETTERAD UTFIL                                         
003410     SELECT W42834                     ASSIGN TO W42833D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  INFIL                                                                
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004006*01  -COPY W407R32A      -L.                                              
004007     SKIP3                                                                
004008 FD  W42834                                                               
004009     RECORDING       F                                                    
004010     BLOCK CONTAINS  0.                                                   
004011                                                                          
004020*01  POST -COPY W407R32A -PRE  UT-  -L.                                   
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W4283300'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004801                                                                          
004802 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
004810     88  END-OF-INFIL                        VALUE 'J'.                   
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007407 01  IN-AREA-START               PIC X(24)   VALUE                        
007408                                 'IN-AREA-START  '.                       
007409     SKIP2                                                                
007410                                                                          
007411*01  AREA -COPY W407R32A     -PRE IN-                                     
007412     EJECT                                                                
007413 01  UT-AREA-START               PIC X(24)   VALUE                        
007414                                 'UT-AREA-START  '.                       
007415     SKIP2                                                                
007416                                                                          
007420*01  AREA -COPY W407R32A     -PRE UT-                                     
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008200     03  W-IDLEVANM-X.                                                    
008201         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
008202         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
008203         05  W-IDRAPPNR          PIC  9(7)   VALUE ZERO.                  
008204                                                                          
008205     03  W-WDA211KY-X.                                                    
008206         05  W-IDARTNR-A211      PIC S9(9)   VALUE ZERO COMP-3.           
008207         05  W-IDRADNR-A211      PIC S9(5)   VALUE ZERO COMP-3.           
008208                                                                          
008215     03  W-IDARTNR-X.                                                     
008216         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008217                                                                          
008218     03  W-KDSEGKEY-X.                                                    
008219         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
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
010101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA201'.                      
010102 01  DLI-IO-WDA201.                                                       
010103*    03  -COPY WDA201                                                     
010104     EJECT                                                                
010105 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA211'.                      
010106 01  DLI-IO-WDA211.                                                       
010107*    03  -COPY WDA211                                                     
010108 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
010109 01  DLI-IO-WDK601.                                                       
010110*    03  -COPY WDK601                                                     
010111     EJECT                                                                
010112 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010113 01  DLI-IO-WDK611.                                                       
010120*    03  -COPY WDK611                                                     
010400     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010600                                                                          
010701                                                                          
010702*01  -COPY W0008  -PRE WDA2-                                              
010703     05  FILLER                  PIC X.                                   
010704                                                                          
010705*01  -COPY W0008  -PRE WDK6-                                              
010710     05  FILLER                  PIC X.                                   
010800     EJECT                                                                
010901 PROCEDURE DIVISION  USING WDA2-PCB WDK6-PCB.                             
010902 MAIN SECTION.                                                            
010910     ENTRY 'DLITCBL' USING WDA2-PCB WDK6-PCB.                             
011000                                                                          
011200                                                                          
011300     PERFORM A-INIT                                                       
011400                                                                          
011510     PERFORM S01-LAES-INFIL                                               
011600     PERFORM UNTIL END-OF-INFIL                                           
011610                                                                          
011700       MOVE IN-R32-IDDISTR   TO W-IDDISTR                                 
011710       MOVE IN-R32-IDKUNDNR  TO W-IDKUNDNR                                
011720       MOVE IN-R32-IDRAPPNR  TO W-IDRAPPNR                                
011730                                                                          
011740       PERFORM B-BEHANDLA                                                 
011750                                                                          
012310       PERFORM S01-LAES-INFIL                                             
012400     END-PERFORM                                                          
012600                                                                          
012700     PERFORM Z-FINIT                                                      
012800                                                                          
012900     MOVE ZERO TO RETURN-CODE                                             
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013300 A-INIT SECTION.                                                          
013401                                                                          
013410     OPEN INPUT  INFIL                                                    
013501                                                                          
013510     OPEN OUTPUT W42834                                                   
013600                                                                          
013700     ACCEPT DAGENS-DATUM  FROM DATE                                       
013810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014110 B-BEHANDLA SECTION.                                                      
014120                                                                          
014121     MOVE IN-AREA   TO UT-AREA                                            
014124                                                                          
014125     PERFORM IMS-GU-WDA201                                                
014126     PERFORM IMS-GNP-WDA211                                               
014127     PERFORM UNTIL SEGMENT-SAKNAS                                         
014128       IF LEV-KDKREBEH(1:1) = 'Y' OR                                      
014129          LEV-KDKREBEH(1:1) = 'J' OR                                      
014130          LEV-KDKREBEH(1:1) = 'C'                                         
014131                                                                          
014132         MOVE LEV-IDARTNR       TO W-IDARTNR                              
014133                                                                          
014134         PERFORM IMS-GU-WDK601                                            
014135         PERFORM IMS-GNP-WDK611                                           
014136         IF SEGMENT-FINNS                                                 
014137           IF IN-R32-KDLEVANM = '7'                                       
014138             COMPUTE UT-R32-SUSTDTOT = UT-R32-SUSTDTOT +                  
014139               ((LEV-KVRETINL + LEV-KVRETINL-SKR) * CLAG-PRARTSTD)        
014140             END-COMPUTE                                                  
014141           ELSE                                                           
014142             COMPUTE UT-R32-SUSTDTOT = UT-R32-SUSTDTOT +                  
014143                               (LEV-KVLEVANM-BEKR * CLAG-PRARTSTD)        
014144             END-COMPUTE                                                  
014145           END-IF                                                         
014146         END-IF                                                           
014147       END-IF                                                             
014148       PERFORM IMS-GNP-WDA211                                             
014149     END-PERFORM                                                          
014150                                                                          
014151     PERFORM  S11-SKRIV-W42834                                            
014152                                                                          
014153     .                                                                    
014160     EJECT                                                                
014200 Z-FINIT SECTION.                                                         
014301     CLOSE INFIL                                                          
014310           W42834                                                         
014401     SKIP2                                                                
014402     MOVE 'S' TO POSTSUM-OPKOD                                            
014410     CALL POSTSUM USING POSTSUM-PARM                                      
014500     .                                                                    
014601     EJECT                                                                
014602 S01-LAES-INFIL   SECTION.                                                
014603     READ INFIL INTO IN-AREA                                              
014604     AT END                                                               
014605        MOVE HIGH-VALUE TO IN-AREA                                        
014606        SET END-OF-INFIL TO TRUE                                          
014607                                                                          
014608     NOT AT END                                                           
014609        MOVE 'INFIL'     TO POSTSUM-FDNAMN                                
014610        MOVE 'W42833D1'  TO POSTSUM-DDNAMN2                               
014613        MOVE SPACE       TO POSTSUM-TRANSTYP                              
014614        CALL POSTSUM USING POSTSUM-PARM                                   
014615     END-READ                                                             
014620     .                                                                    
014701     EJECT                                                                
014702 S11-SKRIV-W42834 SECTION.                                                
014703                                                                          
014704     WRITE UT-POST FROM UT-AREA                                           
014705                                                                          
014706     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
014707     MOVE 'W42834'    TO POSTSUM-FDNAMN                                   
014708     MOVE 'W42833D2'  TO POSTSUM-DDNAMN2                                  
014709     CALL POSTSUM USING POSTSUM-PARM                                      
014710     .                                                                    
014900     EJECT                                                                
015600* --- IMS SEKTIONER ---                                                   
015700                                                                          
015801     EJECT                                                                
015802 IMS-GU-WDA201 SECTION.                                                   
015803                                                                          
015804     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
015805          DELIMITED BY SIZE INTO SSA1                                     
015806     MOVE '  GE' TO GODK-STATUSKODER                                      
015807     CALL CBLTDLI USING GU WDA2-PCB DLI-IO-WDA201 SSA1                    
015808     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
015809     PERFORM IMS-STATUSKONTROLL                                           
015810     .                                                                    
015811     EJECT                                                                
015812 IMS-GNP-WDA211 SECTION.                                                  
015813                                                                          
015814     MOVE 'WDA211   ' TO SSA1                                             
015816     MOVE '  GE' TO GODK-STATUSKODER                                      
015817     CALL CBLTDLI USING GNP WDA2-PCB DLI-IO-WDA211 SSA1                   
015818     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
015819     PERFORM IMS-STATUSKONTROLL                                           
015820     .                                                                    
015821     EJECT                                                                
015822 IMS-GU-WDK601 SECTION.                                                   
015823                                                                          
015824     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
015825          DELIMITED BY SIZE INTO SSA1                                     
015826     MOVE '  GE' TO GODK-STATUSKODER                                      
015827     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
015828     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
015829     PERFORM IMS-STATUSKONTROLL                                           
015830     .                                                                    
015831     EJECT                                                                
015832 IMS-GNP-WDK611 SECTION.                                                  
015833                                                                          
015834     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
015835          DELIMITED BY SIZE INTO SSA1                                     
015836     MOVE '  GE' TO GODK-STATUSKODER                                      
015837     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
015838     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
015839     PERFORM IMS-STATUSKONTROLL                                           
015840     .                                                                    
015900     EJECT                                                                
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
