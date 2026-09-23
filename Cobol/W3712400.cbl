001100 ID DIVISION.                                                             
001200                                                                          
001300 PROGRAM-ID.     W3712400.                                                
001400 AUTHOR.         INGVAR SKJELBRED.                                        
001500 DATE-WRITTEN.   97/06/25.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        TAR BORT RAPPORTER SOM SAKNAR RAPPORTRADER PÅ WDM601             
002100*        DET KOMMER EN FEL FIL FRÅN PGM W37116 SOM LÄGGER UPP             
002110*        NYA RAPPORTER PÅ WDM6.                                           
002120*        HAR EN RAPPORT RAD ETT FELAKTIGT ARTIKELNR LÄGGS DEN             
002130*        RAD EJ UPP PÅ WDM611, MEN DÅ HAR REDAN EN RAPPORT SKAPATS        
002140*        PÅ WDM601 DENNA TAS NU BORT I DETTA PROGRAM MED HJÄLP AV         
002150*        INFILEN W371FEL                                                  
002200*                                                                         
002310*        PROGRAMMET UPPDATERAR WLBYTF (WDM6)                              
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
003602*          --- FELAKTIGA ARTIKELNR PÅ BYTESRAPPORTER                      
003610     SELECT W371FEL                    ASSIGN TO W37124D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201     SKIP3                                                                
004202 FD  W371FEL                                                              
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205                                                                          
004210*01  -COPY W37116      -L.                                                
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004501                                                                          
004510*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W3712400'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900     SKIP2                                                                
005000 01  FELTEXT.                                                             
005100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005401                                                                          
005402 77  W371FEL-EOF-SW              PIC X       VALUE 'N'.                   
005410     88  END-OF-W371FEL                      VALUE 'J'.                   
005700     EJECT                                                                
005800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES DAGENS-DATUM.                                       
006000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006300     EJECT                                                                
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
007202 01  FEL-AREA-START              PIC X(24)   VALUE                        
007203                                             'FEL-AREA-START'.            
007204     SKIP2                                                                
007205                                                                          
007210*01  AREA -COPY W37116     -PRE FEL-                                      
007300*                                                                         
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007600     SKIP3                                                                
007700 01  NYCKLAR-TILL-DLI.                                                    
007800     03  W-WDM601KY-X.                                                    
007801         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
007802         05  W-IDBYTRAP          PIC S9(7)    VALUE ZERO COMP-3.          
007803                                                                          
007804     03  W-WDM611KY-X.                                                    
007805         05  W-IDARTNR-OBJ       PIC S9(9)   VALUE ZERO COMP-3.           
007806         05  W-IDTABNR           PIC S9(3)   VALUE ZERO COMP-3.           
007807                                                                          
007808     03  W-IDBYTRAD-X.                                                    
007809         05  W-IDBYTRAD          PIC S9(5)   VALUE ZERO COMP-3.           
007810                                                                          
007820     03  W-IDBYTRAD-MIN-X.                                                
007830         05  W-IDBYTRAD-MIN      PIC S9(5)   VALUE ZERO COMP-3.           
007840                                                                          
007850     03  W-IDBYTRAD-MAX-X.                                                
007860         05  W-IDBYTRAD-MAX      PIC S9(5)   VALUE ZERO COMP-3.           
007870                                                                          
007880                                                                          
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
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBYTF01'.                    
010002 01  DLI-IO-WLBYTF01.                                                     
010003*    03  -COPY WDM601  -PRE BYTF-                                         
010004     EJECT                                                                
010005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBYTF11'.                    
010006 01  DLI-IO-WLBYTF11.                                                     
010010*    03  -COPY WDM611  -PRE BYTF-                                         
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010600*01  -COPY W0009   -PRE MSG-                                              
010701     EJECT                                                                
010702*01  -COPY W0008  -PRE BYTF-                                              
010710     05  FILLER                  PIC X.                                   
011000     EJECT                                                                
011101 PROCEDURE DIVISION  USING MSG-PCB BYTF-PCB.                              
011102 MAIN SECTION.                                                            
011110     ENTRY 'DLITCBL' USING MSG-PCB BYTF-PCB.                              
011200                                                                          
011400     SKIP2                                                                
011500     PERFORM A-INIT                                                       
011610     PERFORM S01-LAES-W371FEL                                             
011700     PERFORM UNTIL END-OF-W371FEL                                         
011710       PERFORM B-BEARBETA-FELFIL                                          
012410       PERFORM S01-LAES-W371FEL                                           
012500     END-PERFORM                                                          
012600                                                                          
012700                                                                          
012800     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013500     SKIP2                                                                
013601                                                                          
013610     OPEN INPUT W371FEL                                                   
014100                                                                          
014210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014500     .                                                                    
014600     EJECT                                                                
014610 B-BEARBETA-FELFIL SECTION.                                               
014611     DISPLAY 'B-BEARBETA-FELFIL'                                          
014620     SKIP2                                                                
014621                                                                          
014630     MOVE FEL-IDDISTR          TO W-IDDISTR                               
014631     MOVE FEL-IDBYTRAP         TO W-IDBYTRAP                              
014634     MOVE ZERO                 TO W-IDBYTRAD-MIN                          
014635     MOVE +99999               TO W-IDBYTRAD-MAX                          
014636     DISPLAY 'DIST ' FEL-IDDISTR                                          
014637     DISPLAY 'RAPP ' FEL-IDBYTRAP                                         
014638     PERFORM IMS-GET-BYTF-WLBYTF01                                        
014639     IF SEGMENT-FINNS                                                     
014640        DISPLAY 'RAPPORT FINNS '                                          
014641        PERFORM IMS-GET-BYTF-WLBYTF11                                     
014642        IF SEGMENT-SAKNAS                                                 
014643           DISPLAY 'RAPPORT RAD SAKANAS '                                 
014644           PERFORM IMS-GHU-BYTF-WLBYTF01                                  
014646           PERFORM IMS-DLET-BYTF-WLBYTF01                                 
014647        END-IF                                                            
014648     ELSE                                                                 
014649        DISPLAY 'RAPPORT SAKNAS'                                          
014650     END-IF                                                               
014716                                                                          
014728     .                                                                    
014729     EJECT                                                                
014730 Z-FINIT SECTION.                                                         
014800                                                                          
014901                                                                          
014910     CLOSE W371FEL                                                        
015101     SKIP2                                                                
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015300     .                                                                    
015401     EJECT                                                                
015402 S01-LAES-W371FEL  SECTION.                                               
015403     SKIP2                                                                
015404     READ W371FEL INTO FEL-AREA                                           
015405     AT END                                                               
015407        SET END-OF-W371FEL TO TRUE                                        
015408                                                                          
015409     NOT AT END                                                           
015410        MOVE 'W371FEL' TO POSTSUM-FDNAMN                                  
015411        MOVE 'W37124D1' TO POSTSUM-DDNAMN2                                
015412        MOVE FEL-IDPTYP TO POSTSUM-TRANSTYP                               
015413        CALL POSTSUM USING POSTSUM-PARM                                   
015414     END-READ                                                             
015420     .                                                                    
015700     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900     SKIP3                                                                
016001     EJECT                                                                
016002 IMS-GET-BYTF-WLBYTF01 SECTION.                                           
016003                                                                          
016004     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
016005          DELIMITED BY SIZE INTO SSA1                                     
016006     MOVE '  GE' TO GODK-STATUSKODER                                      
016007     CALL CBLTDLI USING GU BYTF-PCB DLI-IO-WLBYTF01 SSA1                  
016008     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
016009     PERFORM IMS-STATUSKONTROLL                                           
016010     .                                                                    
016011     SKIP3                                                                
016012 IMS-GHU-BYTF-WLBYTF01 SECTION.                                           
016013                                                                          
016014     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
016015          DELIMITED BY SIZE INTO SSA1                                     
016016     MOVE '  GE' TO GODK-STATUSKODER                                      
016017     CALL CBLTDLI USING GHU BYTF-PCB DLI-IO-WLBYTF01 SSA1                 
016018     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
016019     PERFORM IMS-STATUSKONTROLL                                           
016020     .                                                                    
016021     SKIP3                                                                
016022 IMS-DLET-BYTF-WLBYTF01 SECTION.                                          
016023                                                                          
016024     MOVE '  ' TO GODK-STATUSKODER                                        
016025     CALL CBLTDLI USING DLET BYTF-PCB DLI-IO-WLBYTF01                     
016026     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
016027     PERFORM IMS-STATUSKONTROLL                                           
016028     .                                                                    
016029     EJECT                                                                
016030 IMS-GET-BYTF-WLBYTF11 SECTION.                                           
016031                                                                          
016032     STRING 'WLBYTF11(IDBYTRAD >' W-IDBYTRAD-MIN-X                        
016033                    '&IDBYTRAD <' W-IDBYTRAD-MAX-X ')'                    
016034          DELIMITED BY SIZE INTO SSA1                                     
016035     MOVE '  GE' TO GODK-STATUSKODER                                      
016036     CALL CBLTDLI USING GNP BYTF-PCB DLI-IO-WLBYTF11 SSA1                 
016037     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
016038     PERFORM IMS-STATUSKONTROLL                                           
016039     .                                                                    
016040     SKIP3                                                                
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
