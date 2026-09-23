001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4766100.                                                
001300 AUTHOR.         MOGREN STINA.                                            
001400 DATE-WRITTEN.   05/10/28.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001720*                                                                         
001800*        PROGRAMMET STARTAS EFTER MPP 4634 (SAVE-IT)                      
001810*        VIA SOP-RUTIN W476SX                                             
001820*        RADPOSTER FRÅN BILLIT ÄR 'PARAMETER' IN                          
001821*        DETTA PROGRAM SKALL BARA KOLLA OM DET FINNS                      
001822*        RADER OCH DÅ BEORDRA RUTIN W476S5                                
001840*                                                                         
001900*                                                                         
002010*        PROGRAMMET LÄSER         R4  FAKTURAINFO                         
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003305*          --- FIL MED SVAR-POST                                          
003310     SELECT W47661                     ASSIGN TO W47661D1.                
003500     SKIP2                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP2                                                                
003908 FD  W47661                                                               
003909     RECORDING       F                                                    
003910     BLOCK CONTAINS  0.                                                   
003911                                                                          
003920*01  POST -COPY W4766601  -PRE  UT-  -L.                                  
004000     EJECT                                                                
004060                                                                          
004130 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4766100'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600                                                                          
004720 77  FIL61-SW                    PIC X       VALUE 'N'.                   
004730     88  FIL61                               VALUE 'J'.                   
004800                                                                          
004813 77  W-ANT                       PIC S9(3)   VALUE ZERO COMP-3.           
004817                                                                          
004818 01  ERRTEXT.                                                             
004819     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
004820     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
004821                                                                          
004822*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
004823 77  W-TIAAMMDD                  PIC 9(6)    VALUE ZERO.                  
004824 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
004825                                                                          
004841*                                                                         
004844                                                                          
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005400     EJECT                                                                
005408                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006001     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
006010     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006100     SKIP2                                                                
006121                                                                          
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007100 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
007101                                                                          
007139*    --- PARAMETRAR TILL POSTSUM                                          
007140*                                                                         
007150*01  -COPY W0005   -PRE  POSTSUM-                                         
007201     EJECT                                                                
007210*01  -COPY WDATAREA                                                       
007301     EJECT                                                                
007302 01  UT-AREA-START            PIC X(16)  VALUE 'UT-AREA-START '.          
007305                                                                          
007310*01   -COPY W4766601                                                      
007311*********  AREOR  FÖR  FIL61                                              
007460     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008247                                                                          
008248     03  W-WDGXKEY-4507-X.                                                
008249         05  W-IDHTYP-4507       PIC X(4)    VALUE '4507'.                
008250         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008251     03  W-IDFAKT-4508-X.                                                 
008252         05  W-IDFAKT-4508       PIC S9(7)   VALUE ZERO COMP-3.           
008253     03  W-IDFAKT-4508-MIN-X.                                             
008254         05  W-IDFAKT-4508-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
008255     03  W-IDFAKT-4508-MAX-X.                                             
008256         05  W-IDFAKT-4508-MAX   PIC S9(7)   VALUE ZERO COMP-3.           
008257     03  W-FLKLAR-X.                                                      
008258         05  W-FLKLAR            PIC X(1)    VALUE 'J'.                   
008276*                                                                         
008280     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008710     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008720     88  IMS-NOT-OK                          VALUE 'XD'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
009910 01  FILLER                      PIC X(16)   VALUE 'IO-4507  '.           
010000 01  DLI-IO-4507.                                                         
010100*    03  -COPY  WDGX01                                                    
010351 01  FILLER                      PIC X(16)   VALUE 'IO-4508  '.           
010352 01  DLI-IO-4508.                                                         
010353*    03  -COPY WDGX4508                                                   
010404                                                                          
010405 01  IO-PCB                      PIC X.                                   
010410 LINKAGE SECTION.                                                         
010500                                                                          
010697*01  -COPY W0008  -PRE 4507-                                              
010698     05  FILLER                  PIC X.                                   
010800     EJECT                                                                
010801 PROCEDURE DIVISION  USING 4507-PCB.                                      
010831 MAIN SECTION.                                                            
010832     ENTRY 'DLITCBL' USING 4507-PCB.                                      
010900                                                                          
011200     PERFORM A-INIT                                                       
011210                                                                          
011220     PERFORM IMS-GU-WDGX4507                                              
011300                                                                          
011320     PERFORM IMS-GNP-WDGX4508                                             
011340     IF SEGMENT-FINNS                                                     
011341       MOVE 4508-IDFAKT        TO W-IDFAKT-4508                           
011342                                  RJX-IDFAKT                              
012100                                                                          
012200       PERFORM S01-SKRIV-W47661                                           
012310     END-IF                                                               
012400                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013401     MOVE 'A-INIT'              TO WS-SEKTION                             
013404                                                                          
013405     OPEN OUTPUT W47661                                                   
013406     MOVE SPACE                 TO RJX-W4766601                           
013500                                                                          
013600     ACCEPT DAGENS-DATUM         FROM DATE                                
013700     ACCEPT WS-TTMMSSTH          FROM TIME                                
013701     MOVE DAGENS-DATUM           TO RJX-IDTRPBON                          
013702     MOVE WS-TTMMSSTH            TO RJX-FILLER                            
013703     ACCEPT W-TIAAMMDD           FROM DATE                                
013710     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
013720                                                                          
013760                                                                          
013770     MOVE +1                     TO W-IDFAKT-4508-MIN                     
013780     MOVE +9999999               TO W-IDFAKT-4508-MAX                     
013790                                                                          
013900     .                                                                    
014000     EJECT                                                                
015505 Z-FINIT SECTION.                                                         
015506     MOVE 'Z-FINIT'              TO WS-SEKTION                            
015507                                                                          
015522     CLOSE W47661                                                         
015529                                                                          
015530     MOVE 'S'                    TO POSTSUM-OPKOD                         
015531     CALL POSTSUM                USING POSTSUM-PARM                       
015532     .                                                                    
015540     EJECT                                                                
015688 S01-SKRIV-W47661 SECTION.                                                
015689*                                                                         
015690     WRITE UT-POST               FROM RJX-W4766601                        
015691                                                                          
015692     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
015693     MOVE 'W47661'               TO POSTSUM-FDNAMN                        
015694     MOVE 'W47661D1'             TO POSTSUM-DDNAMN2                       
015695     CALL POSTSUM                USING POSTSUM-PARM                       
015696     .                                                                    
015697     EJECT                                                                
015939* --- IMS SEKTIONER ---                                                   
015940                                                                          
016130 IMS-GU-WDGX4507  SECTION.                                                
016131     MOVE 'GU-WDGX4507'     TO WS-SEKTION                                 
016132                                                                          
016133     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4507-X ')'                    
016134          DELIMITED BY SIZE INTO SSA1                                     
016135     MOVE '    '                 TO GODK-STATUSKODER                      
016136     CALL CBLTDLI USING GU 4507-PCB DLI-IO-4507 SSA1                      
016137     MOVE 4507-STATUS-CODE       TO STATUS-WS                             
016138     PERFORM IMS-STATUSKONTROLL                                           
016139     .                                                                    
016140     SKIP3                                                                
016141 IMS-GNP-WDGX4508  SECTION.                                               
016142     MOVE 'GNP-WDGX4508'   TO WS-SEKTION                                  
016143                                                                          
016144     STRING 'WDGX4508(IDFAKT  >=' W-IDFAKT-4508-MIN-X                     
016145                    '&IDFAKT  <=' W-IDFAKT-4508-MAX-X                     
016146                    '&FLKLAR   =' W-FLKLAR-X ')'                          
016147          DELIMITED BY SIZE INTO SSA1                                     
016148     MOVE '  GE'               TO GODK-STATUSKODER                        
016149     CALL CBLTDLI USING GNP 4507-PCB DLI-IO-4508 SSA1                     
016150     MOVE 4507-STATUS-CODE       TO STATUS-WS                             
016151     PERFORM IMS-STATUSKONTROLL                                           
016152     .                                                                    
016153     SKIP3                                                                
016261 IMS-STATUSKONTROLL SECTION.                                              
016262                                                                          
016263     SET STATUS-IX TO 1                                                   
016270     SEARCH GODK-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016500           DELIMITED BY SIZE INTO FELTEXT                                 
016700         CALL FELLOG                                                      
016800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
