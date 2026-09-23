001000************************************                                      
001100 ID DIVISION.                                                             
001200                                                                          
001300 PROGRAM-ID.     W6114900.                                                
001400 AUTHOR.         KENT JEBSEN.                                             
001500 DATE-WRITTEN.   97/05/07.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        BYTER FÖR NDC:ERNA UT STANDARDPRIS MOT BESTÄLLNINGSPRIS          
002100*        SOM HÄMTAS FRÅN WDK621.                                          
002200*                                                                         
002310*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  . . . .                                                 
002700*        U1000 -  . . . .                                                 
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003601     SKIP2                                                                
003602*          --- SB AV W6D1 (FRÅN W61110)                                   
003603     SELECT W61110                     ASSIGN TO W61149D1.                
003604     SKIP2                                                                
003605*          --- STDPRIS UTBYTT MOT BEST-PRIS FÖR NDC                       
003610     SELECT W61149                     ASSIGN TO W61149D2.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201     SKIP3                                                                
004202 FD  W61110                                                               
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205                                                                          
004206*01  -COPY W6111001      -L.                                              
004207     SKIP3                                                                
004208 FD  W61149                                                               
004209     RECORDING       F                                                    
004210     BLOCK CONTAINS  0.                                                   
004211                                                                          
004220*01  POST -COPY W6111001 -PRE  UT-  -L.                                   
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004501                                                                          
004510*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W6114900'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900     SKIP2                                                                
005000 01  FELTEXT.                                                             
005100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005401                                                                          
005402 77  W61110-EOF-SW               PIC X       VALUE 'N'.                   
005410     88  END-OF-W61110                       VALUE 'J'.                   
005700     EJECT                                                                
005800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES DAGENS-DATUM.                                       
006000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006300     EJECT                                                                
006310*      --- VALID IDDC CODES                                               
006320*                                                                         
006330*01    -COPY WWDC99                                                       
006340       EJECT                                                              
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500*                                                                         
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006810     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006901     EJECT                                                                
006902*    --- PARAMETRAR TILL POSTSUM                                          
006903*                                                                         
006910*01  -COPY W0005   -PRE  POSTSUM-                                         
007201     EJECT                                                                
007202 01  IN-AREA-START               PIC X(24)   VALUE                        
007203                                             'IN-AREA-START'.             
007204     SKIP2                                                                
007205                                                                          
007206*01  AREA -COPY W6111001     -PRE IN-                                     
007207     EJECT                                                                
007208 01  UT-AREA-START               PIC X(24)   VALUE                        
007209                                             'UT-AREA-START'.             
007210     SKIP2                                                                
007211                                                                          
007220*01  AREA -COPY W6111001     -PRE UT-                                     
007300*                                                                         
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007600     SKIP3                                                                
007700 01  NYCKLAR-TILL-DLI.                                                    
007801     03  W-IDARTNR-X.                                                     
007802         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007806     03  W-WDK621KY-X.                                                    
007807         05  W-DAPRLIST-9KOMPL   PIC 9(8)    VALUE ZERO.                  
007808         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
007900     SKIP2                                                                
008000*    --- STATUS-KOD FRÅN IMS                                              
008100 01  STATUS-WS                   PIC XX.                                  
008200     88  SEGMENT-FINNS                       VALUE '  '.                  
008300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008600     88  IMS-EJ-OK                           VALUE 'XD'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009200 01  SSA2                        PIC X(64).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009900                                                                          
010020 01  DLI-IO-AREA.                                                         
010030     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
010040     SKIP3                                                                
010050     03  WLARTC01 REDEFINES IO-AREA.                                      
010060*        05  -COPY WDK601  -PRE ARTC-                                     
010070     EJECT                                                                
010080     03  WLARTC11 REDEFINES IO-AREA.                                      
010090*        05  -COPY WDK611  -PRE ARTC-                                     
010100     EJECT                                                                
010200     03  WLARTC21 REDEFINES IO-AREA.                                      
010210*        05  -COPY WDK621  -PRE ARTC-                                     
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010702*01  -COPY W0008   -PRE ARTC-                                             
010710     05  FILLER                  PIC X.                                   
011000     EJECT                                                                
011101 PROCEDURE DIVISION  USING ARTC-PCB.                                      
011102 MAIN SECTION.                                                            
011110     ENTRY 'DLITCBL' USING ARTC-PCB.                                      
011200                                                                          
011400     SKIP2                                                                
011500     PERFORM A-INIT                                                       
011610     PERFORM S01-LAES-W61110                                              
011700     PERFORM UNTIL END-OF-W61110                                          
011710       MOVE IN-AREA  TO UT-AREA                                           
011720       MOVE IN-IDDC  TO WS-IDDC                                           
011800       IF NDC-NA                                                          
011900         PERFORM B-HAMTA-BEST-PRIS                                        
012000       END-IF                                                             
012110       PERFORM S11-SKRIV-W61149                                           
012410       PERFORM S01-LAES-W61110                                            
012500     END-PERFORM                                                          
012600                                                                          
012800     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013500     SKIP2                                                                
013601                                                                          
013610     OPEN INPUT W61110                                                    
013701                                                                          
013710     OPEN OUTPUT W61149                                                   
013900                                                                          
014100                                                                          
014210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014500     .                                                                    
014600     EJECT                                                                
014610 B-HAMTA-BEST-PRIS SECTION.                                               
014640                                                                          
014650     MOVE IN-IDARTNR TO W-IDARTNR                                         
014652     PERFORM IMS-GU-ARTC11                                                
014653     IF SEGMENT-FINNS                                                     
014654       PERFORM IMS-GET-ARTC-PRL                                           
014660       IF SEGMENT-FINNS                                                   
014680         MOVE ARTC-PRL-PRARTBES-PR TO UT-PRARTSTD                         
014690       END-IF                                                             
014691     END-IF                                                               
014692     .                                                                    
014693     EJECT                                                                
014700 Z-FINIT SECTION.                                                         
014800                                                                          
014901                                                                          
014902     CLOSE W61110                                                         
014903                                                                          
014910           W61149                                                         
015101     SKIP2                                                                
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015300     .                                                                    
015401     EJECT                                                                
015402 S01-LAES-W61110  SECTION.                                                
015403     SKIP2                                                                
015404     READ W61110 INTO IN-AREA                                             
015405     AT END                                                               
015407        SET END-OF-W61110 TO TRUE                                         
015408                                                                          
015409     NOT AT END                                                           
015410        MOVE 'W61110'   TO POSTSUM-FDNAMN                                 
015411        MOVE 'W61149D1' TO POSTSUM-DDNAMN2                                
015413        CALL POSTSUM    USING POSTSUM-PARM                                
015414     END-READ                                                             
015420     .                                                                    
015501     EJECT                                                                
015502 S11-SKRIV-W61149 SECTION.                                                
015503     SKIP2                                                                
015504     WRITE UT-POST   FROM UT-AREA                                         
015505                                                                          
015507     MOVE 'W61149 '     TO POSTSUM-FDNAMN                                 
015508     MOVE 'W61149D2'    TO POSTSUM-DDNAMN2                                
015509     CALL POSTSUM       USING POSTSUM-PARM                                
015510     .                                                                    
015700     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900     SKIP3                                                                
016001     EJECT                                                                
016012 IMS-GU-ARTC11 SECTION.                                                   
016013     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
016014          DELIMITED BY SIZE INTO SSA1                                     
016015     MOVE 'WLARTC11'       TO SSA2                                        
016016     MOVE '  GE' TO GODK-STATUSKODER                                      
016018     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
016019     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
016020     PERFORM IMS-STATUSKONTROLL                                           
016021     .                                                                    
016022     EJECT                                                                
016023 IMS-GET-ARTC-PRL SECTION.                                                
016024                                                                          
016025     STRING 'WLARTC21(WDK621KY>=' W-WDK621KY-X ')'                        
016026          DELIMITED BY SIZE INTO SSA1                                     
016027     MOVE '  GE' TO GODK-STATUSKODER                                      
016028     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
016029     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
016030     PERFORM IMS-STATUSKONTROLL                                           
016040     .                                                                    
016100     EJECT                                                                
016200 IMS-STATUSKONTROLL SECTION.                                              
016300     SKIP2                                                                
016400     SET STATUS-IX TO 1                                                   
016500     SEARCH GODK-STATUS                                                   
016600       AT END                                                             
016700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016800           DELIMITED BY SIZE INTO FELTEXT                                 
016900         DISPLAY FELTEXT                                                  
017000         CALL FELLOG                                                      
017100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017200         CONTINUE                                                         
017300     END-SEARCH                                                           
017400     .                                                                    
