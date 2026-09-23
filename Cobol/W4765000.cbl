000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4765000.                                                
000400*AUTHOR.         STINA MOGREN.                                            
000500*DATE-WRITTEN.   02/06/24.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER NER LOGGTRANSAR FRÅN SAVEIT TILL SEKV.FIL.                 
001010*                                                                         
001020*        SKAPAR DESSUTOM FIL FÖR ATT RENSA WDR7                           
001021*                                                                         
001030*       (TILL DETTA PROGRAM KOMMER ENDAST TYP '510'                       
001040*        ÖVRIGA TYPER  SKAPAS I W4764900)                                 
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDR7 MED SB.                               
001300*                                                                         
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- TRANSAR                                                    
003100     SELECT W47601                     ASSIGN TO W47650D1.                
003200     SKIP2                                                                
003600*          --- WDR7 TRANSAR SOM SKALL RENSAS                              
003700     SELECT W47604                     ASSIGN TO W47650D2.                
003710     SKIP2                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W47601                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600     SKIP2                                                                
004700*01  POST -COPY W4765001  -PRE  UT-   -L.                                 
004800     SKIP3                                                                
005700 FD  W47604                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000     SKIP2                                                                
006100*01  POST -COPY WDR701    -PRE  RENS-  -L.                                
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400     SKIP2                                                                
006401                                                                          
006410*    -- CHECKED BY WY2000                                                 
006500 77  IDPGM                       PIC X(8)    VALUE 'W4765000'.            
006600 77  JA                          PIC X       VALUE 'J'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800     EJECT                                                                
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007500     SKIP2                                                                
007600*    --- PARAMETRAR TILL ABEND                                            
007700                                                                          
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000     SKIP2                                                                
008010 01  W-IDFAKT                    PIC S9(7)  VALUE ZERO COMP-3.            
008020 77  W-FLKLAR                    PIC X      VALUE 'N'.                    
008021     88 FAKT-OK                  VALUE 'J'.                               
008030                                                                          
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008820                                                                          
008900 01  W4760001-AREA-START           PIC X(24)   VALUE                      
009000                                 'W4760001-AREA-START  '.                 
009100     SKIP2                                                                
009200*01  AREA   -COPY W4765001  -PRE UT-                                      
009300     EJECT                                                                
009310                                                                          
009810                                                                          
010400 01  WDR701-AREA-START           PIC X(24)   VALUE                        
010500                                 'WDR701-AREA-START  '.                   
010600     SKIP2                                                                
010700*01  AREA -COPY WDR701      -PRE RENS-                                    
010800     EJECT                                                                
010900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011000*                                                                         
011010 01  SSA1                        PIC X(64).                               
011020 01  SSA2                        PIC X(64).                               
011030     EJECT                                                                
011100     SKIP2                                                                
011200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011201 01  NYCKLAR-TILL-DLI.                                                    
011210     03  W-WDGXKEY-4507-X.                                                
011220         05  W-IDHTYP-4507       PIC X(4)    VALUE '4507'.                
011230         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
011240     03  W-IDFAKT-4508-X.                                                 
011250         05  W-IDFAKT-4508       PIC S9(7)   VALUE ZERO COMP-3.           
011300     SKIP3                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012210     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDR701'.         
013500     SKIP2                                                                
013600 01  DLI-IO-WDR701.                                                       
013800     SKIP3                                                                
013900*    03  -COPY WDR701                                                     
014100     EJECT                                                                
014200 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDGX4508'.          
014300 01  DLI-IO-WDGX4508.                                                     
014400*    03   -COPY WDGX4508                                                  
014500                                                                          
014800 LINKAGE SECTION.                                                         
014900     SKIP2                                                                
015000*01  -COPY W0008  -PRE WDR7-                                              
015100     05  FILLER                  PIC X.                                   
015200     EJECT                                                                
015210*01  -COPY W0008  -PRE 4507-                                              
015220     05  FILLER                  PIC X.                                   
015230     EJECT                                                                
015300 PROCEDURE DIVISION  USING WDR7-PCB 4507-PCB.                             
015400     ENTRY 'DLITCBL' USING WDR7-PCB 4507-PCB.                             
015500                                                                          
015600     SKIP2                                                                
015700     PERFORM A-INIT                                                       
015800                                                                          
015900     PERFORM IMS-GN-WDR7-FIL                                              
016000     PERFORM UNTIL (NOT SEGMENT-FINNS)                                    
016100                                                                          
016200       IF  FIL-CT-IDSYSTEM = 'W476'                                       
016210         AND FIL-IDPGM = 'W4063400'                                       
016300         EVALUATE FIL-CT-IDPTYP                                           
017240           WHEN '510'                                                     
017250             PERFORM B-SKAPA-POST                                         
017251             IF FAKT-OK                                                   
017252               PERFORM S11-SKRIV-W47601                                   
017260               PERFORM E-SKAPA-RENS-POST                                  
017270             END-IF                                                       
017300           WHEN OTHER                                                     
017400             CONTINUE                                                     
017500         END-EVALUATE                                                     
017600       ELSE                                                               
017700         CONTINUE                                                         
017800       END-IF                                                             
017900                                                                          
018000       PERFORM IMS-GN-WDR7-FIL                                            
018100     END-PERFORM                                                          
018200                                                                          
018300     PERFORM Z-FINIT                                                      
018400                                                                          
018500     MOVE ZERO TO RETURN-CODE                                             
018600     GOBACK                                                               
018700     .                                                                    
018800     EJECT                                                                
018900 A-INIT SECTION.                                                          
019000                                                                          
019100     OPEN OUTPUT W47601                                                   
019300                 W47604                                                   
019400                                                                          
019500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019600     .                                                                    
019700     EJECT                                                                
019800 B-SKAPA-POST SECTION.                                                    
019900                                                                          
020000     MOVE FIL-CT-IDPTYP          TO UT-IDPTYP                             
022900     MOVE FIL-WDR701-DATA        TO UT-W476FAKT                           
023000     IF W-IDFAKT NOT = UT-IDFAKT                                          
023010       MOVE UT-IDFAKT            TO W-IDFAKT-4508                         
023020       PERFORM IMS-GU-WDR4-4508                                           
023030       IF SEGMENT-FINNS                                                   
023051         IF 4508-FLKLAR = NEJ                                             
023052           MOVE NEJ              TO W-FLKLAR                              
023053         ELSE                                                             
023054           MOVE JA               TO W-FLKLAR                              
023055         END-IF                                                           
023056       ELSE                                                               
023057         MOVE JA                 TO W-FLKLAR                              
023060       END-IF                                                             
023061       MOVE UT-IDFAKT            TO W-IDFAKT                              
023070     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
035000 E-SKAPA-RENS-POST SECTION.                                               
035100                                                                          
035200     MOVE FIL-IDPGM              TO RENS-FIL-IDPGM                        
035300     MOVE FIL-TIREGDAT           TO RENS-FIL-TIREGDAT                     
035400     MOVE FIL-TIKLOCK            TO RENS-FIL-TIKLOCK                      
035500     MOVE FIL-IDSEKVNR           TO RENS-FIL-IDSEKVNR                     
035600     MOVE FIL-CT-IDSYSTEM        TO RENS-FIL-CT-IDSYSTEM                  
035700     MOVE FIL-CT-IDPTYP          TO RENS-FIL-CT-IDPTYP                    
035800     MOVE FIL-CT-IDVTYP          TO RENS-FIL-CT-IDVTYP                    
035810     MOVE FIL-WDR701-DATA        TO RENS-FIL-WDR701-DATA                  
035900     PERFORM S14-SKRIV-W47604                                             
036000     .                                                                    
036100     EJECT                                                                
036336 Z-FINIT SECTION.                                                         
036340     CLOSE W47601                                                         
036500           W47604                                                         
036600     SKIP2                                                                
036700     MOVE 'S' TO POSTSUM-OPKOD                                            
036800     CALL POSTSUM USING POSTSUM-PARM                                      
036900     .                                                                    
037000     EJECT                                                                
037120 S11-SKRIV-W47601 SECTION.                                                
037200     SKIP2                                                                
037300     WRITE UT-POST   FROM UT-AREA                                         
037400                                                                          
037500     MOVE UT-IDPTYP        TO POSTSUM-TRANSTYP                            
037600     MOVE 'W47601'         TO POSTSUM-FDNAMN                              
037700     MOVE 'W47650D1'       TO POSTSUM-DDNAMN2                             
037800     CALL POSTSUM USING POSTSUM-PARM                                      
037900     .                                                                    
038000     EJECT                                                                
040100 S14-SKRIV-W47604 SECTION.                                                
040200     SKIP2                                                                
040300     WRITE RENS-POST      FROM RENS-AREA                                  
040400                                                                          
040500     MOVE SPACE           TO POSTSUM-TRANSTYP                             
040600     MOVE 'W47604'        TO POSTSUM-FDNAMN                               
040700     MOVE 'W47650D2'      TO POSTSUM-DDNAMN2                              
040800     CALL POSTSUM USING POSTSUM-PARM                                      
040900     .                                                                    
041000     EJECT                                                                
041110* --- IMS SEKTIONER ---                                                   
041200     SKIP3                                                                
041300 IMS-GN-WDR7-FIL SECTION.                                                 
041500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
041600     CALL CBLTDLI USING GN WDR7-PCB DLI-IO-WDR701                         
041700     MOVE WDR7-STATUS-CODE TO STATUS-WS                                   
041800     PERFORM IMS-STATUSKONTROLL                                           
041900     .                                                                    
042000     SKIP3                                                                
042010 IMS-GU-WDR4-4508 SECTION.                                                
042020*    MOVE 'IMS-GU-WDR4-4508'    TO WS-SEKTION                             
042030                                                                          
042040     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4507-X ')'                    
042050          DELIMITED BY SIZE INTO SSA1                                     
042060     STRING 'WDGX4508*F(IDFAKT   =' W-IDFAKT-4508-X ')'                   
042070          DELIMITED BY SIZE INTO SSA2                                     
042080     MOVE '  GEGB' TO GODK-STATUSKODER                                    
042090     CALL CBLTDLI USING GU 4507-PCB DLI-IO-WDGX4508 SSA1 SSA2             
042091     MOVE 4507-STATUS-CODE TO STATUS-WS                                   
042092     PERFORM IMS-STATUSKONTROLL                                           
042093     .                                                                    
042094     EJECT                                                                
042100 IMS-STATUSKONTROLL SECTION.                                              
042200     SKIP2                                                                
042300     SET STATUS-IX TO 1                                                   
042400     SEARCH GODK-STATUS                                                   
042500       AT END                                                             
042600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
042700           DELIMITED BY SIZE INTO FELTEXT-STR                             
042800         DISPLAY FELTEXT                                                  
042900         CALL FELLOG                                                      
043000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
043100         CONTINUE                                                         
043200     END-SEARCH                                                           
043300     .                                                                    
