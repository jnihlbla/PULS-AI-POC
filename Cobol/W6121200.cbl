001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W6121200.                                                
001200 AUTHOR.         TOMMIE JIVARP.                                           
001300 DATE-WRITTEN.   97/12/10.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        PROGRAMMET SKRIVER UTFILEN W61212 MED LAGERPLATSINFO             
001800*        IFRÅN WDK7.                                                      
001901*                                                                         
001910*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- UTFIL MED LAGERPLATSINFORMATION                            
003203                                                                          
003210     SELECT W61212                     ASSIGN TO W61212D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W61212                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  POST -COPY W61211  -PRE  UT1-  -L.                                   
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W6121200'.            
004300 77  JA                          PIC X       VALUE 'Y'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004700     EJECT                                                                
004800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005300     EJECT                                                                
005301                                                                          
005302*    --- SPARADE VÄRDEN                                                   
005310 01  SPAR-IDARTNR                PIC 9(8)    VALUE ZERO.                  
005320 01  WS-FLSEASON                 PIC X       VALUE 'N'.                   
005330                                                                          
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006000     SKIP2                                                                
006100*    --- PARAMETRAR TILL ABEND                                            
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007201     EJECT                                                                
007202 01  UT1-AREA-START              PIC X(24)   VALUE                        
007203                                 'UT1-AREA-START  '.                      
007204     SKIP2                                                                
007205                                                                          
007210*01  AREA -COPY W61211     -PRE UT1-                                      
007300     EJECT                                                                
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008001     03  W-IDARTNR-X.                                                     
008002         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008003     03  W-IDDC-X.                                                        
008010         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008600     SKIP2                                                                
008700 01  GODK-STATUSKODER.                                                    
008800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008900     SKIP3                                                                
009000 01  SSA1                        PIC X(64).                               
009100 01  SSA2                        PIC X(64).                               
009200     EJECT                                                                
009300*    --- IMS FUNKTIONSKODER                                               
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009801 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA'.                        
009802 01  DLI-IO-AREA.                                                         
009803     03  IO-AREA    PIC X(600) VALUE SPACE.                               
009804         03  DLI-IO-WLARTS01 REDEFINES IO-AREA.                           
009810*            05  -COPY WDK701  -PRE ARTS-                                 
009820         03  DLI-IO-WLARTS11 REDEFINES IO-AREA.                           
009830*            05  -COPY WDK711  -PRE ARTS-                                 
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010300                                                                          
010401     EJECT                                                                
010402*01  -COPY W0008  -PRE ARTS-                                              
010410     05  FILLER                  PIC X.                                   
010500     EJECT                                                                
010601 PROCEDURE DIVISION  USING ARTS-PCB.                                      
010602 MAIN SECTION.                                                            
010610     ENTRY 'DLITCBL' USING ARTS-PCB.                                      
010700                                                                          
010900                                                                          
011000     PERFORM A-INIT                                                       
011100                                                                          
011201     PERFORM IMS-GET-ARTS                                                 
011202     PERFORM UNTIL SEGMENT-SAKNAS                                         
011203       EVALUATE ARTS-SEG-NAME-FB                                          
011204         WHEN 'WDK701'                                                    
011205           MOVE ARTS-SART-IDARTNR  TO SPAR-IDARTNR                        
011206         WHEN 'WDK711'                                                    
011207*          IF ARTS-SLAG-ADLAGOMR = 10 OR 15                               
011208             PERFORM B-KONTROLLERA-RESEASON                               
011209             PERFORM C-FLYTTA-TILL-UT1-AREA                               
011210             PERFORM S11-SKRIV-W61212                                     
011220*          END-IF                                                         
011231       END-EVALUATE                                                       
011233       PERFORM IMS-GET-ARTS                                               
011240     END-PERFORM                                                          
011300     PERFORM Z-FINIT                                                      
011400                                                                          
011500     MOVE ZERO TO RETURN-CODE                                             
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011900 A-INIT SECTION.                                                          
012101                                                                          
012110     OPEN OUTPUT W61212                                                   
012200                                                                          
012300     ACCEPT DAGENS-DATUM  FROM DATE                                       
012410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012600     .                                                                    
012700     EJECT                                                                
012701                                                                          
012710 B-KONTROLLERA-RESEASON SECTION.                                          
012720                                                                          
012730     IF NOT (ARTS-SLAG-RESEASON(1) = 1.00                                 
012740        AND  ARTS-SLAG-RESEASON(2) = 1.00                                 
012750        AND  ARTS-SLAG-RESEASON(3) = 1.00                                 
012760        AND  ARTS-SLAG-RESEASON(4) = 1.00                                 
012770        AND  ARTS-SLAG-RESEASON(5) = 1.00                                 
012780        AND  ARTS-SLAG-RESEASON(6) = 1.00                                 
012790        AND  ARTS-SLAG-RESEASON(7) = 1.00                                 
012791        AND  ARTS-SLAG-RESEASON(8) = 1.00                                 
012792        AND  ARTS-SLAG-RESEASON(9) = 1.00                                 
012793        AND  ARTS-SLAG-RESEASON(10) = 1.00                                
012794        AND  ARTS-SLAG-RESEASON(11) = 1.00                                
012795        AND  ARTS-SLAG-RESEASON(12) = 1.00)                               
012796       MOVE  JA TO  WS-FLSEASON                                           
012797     ELSE                                                                 
012798       MOVE NEJ TO  WS-FLSEASON                                           
012799     END-IF                                                               
012800     .                                                                    
012810     EJECT                                                                
012820                                                                          
012830 C-FLYTTA-TILL-UT1-AREA SECTION.                                          
012840                                                                          
012850     MOVE SPAR-IDARTNR       TO UT1-IDARTNR                               
012860     MOVE ARTS-SLAG-IDDC     TO UT1-IDDC                                  
012870     MOVE ARTS-SLAG-ADLAGOMR TO UT1-ADLAGOMR                              
012880     MOVE ARTS-SLAG-ADGANG   TO UT1-ADGANG                                
012890     MOVE ARTS-SLAG-ADPLATS  TO UT1-ADPLATS                               
012891     MOVE ARTS-SLAG-KVPB-REF TO UT1-KVPB-REF                              
012892     MOVE WS-FLSEASON        TO UT1-FLSEASON                              
012893     .                                                                    
012894     EJECT                                                                
012895                                                                          
012900 Z-FINIT SECTION.                                                         
012910     CLOSE W61212                                                         
013001     SKIP2                                                                
013002     MOVE 'S' TO POSTSUM-OPKOD                                            
013010     CALL POSTSUM USING POSTSUM-PARM                                      
013100     .                                                                    
013301     EJECT                                                                
013302 S11-SKRIV-W61212 SECTION.                                                
013303                                                                          
013304     WRITE UT1-POST FROM UT1-AREA                                         
013305                                                                          
013306     MOVE SPACE    TO POSTSUM-TRANSTYP                                    
013307     MOVE 'W61212' TO POSTSUM-FDNAMN                                      
013308     MOVE 'W61212D1' TO POSTSUM-DDNAMN2                                   
013309     CALL POSTSUM USING POSTSUM-PARM                                      
013310     .                                                                    
013500     EJECT                                                                
013600 S99-ABEND SECTION.                                                       
013700                                                                          
013801     SKIP2                                                                
013802     MOVE 'S' TO POSTSUM-OPKOD                                            
013810     CALL POSTSUM USING POSTSUM-PARM                                      
013900     CALL ABEND USING RKOD-ABEND                                          
014000     .                                                                    
014100     EJECT                                                                
014200* --- IMS SEKTIONER ---                                                   
014300     SKIP3                                                                
014401     EJECT                                                                
014402 IMS-GET-ARTS   SECTION.                                                  
014403                                                                          
014404     CALL CBLTDLI USING GN ARTS-PCB DLI-IO-AREA                           
014405     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
014406     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014407     PERFORM IMS-STATUSKONTROLL                                           
014410     .                                                                    
014500     EJECT                                                                
014600 IMS-STATUSKONTROLL SECTION.                                              
014700                                                                          
014800     SET STATUS-IX TO 1                                                   
014900     SEARCH GODK-STATUS                                                   
015000       AT END                                                             
015100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015200           DELIMITED BY SIZE INTO FELTEXT                                 
015300         DISPLAY FELTEXT                                                  
015400         CALL FELLOG                                                      
015500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015600         CONTINUE                                                         
015700     END-SEARCH                                                           
015800     .                                                                    
