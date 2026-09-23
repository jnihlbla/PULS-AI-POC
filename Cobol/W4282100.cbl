001000 ID DIVISION.                                                             
001100                                                                          
001200 PROGRAM-ID.     W4282100.                                                
001300 AUTHOR.         MÅNS SAMUELSSON.                                         
001400 DATE-WRITTEN.   96/03/12.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        LÄSER NER SÄNDNINGSREGISTRET OCH SKAPAR FIL MED                  
001900*        INFO TILL LEDTIDSBERÄKNING                                       
002000*                                                                         
002110*        PROGRAMMET LÄSER      WLRETA (WDA3)                              
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
003402*          --- FIL MED LEDTIDS INFO                                       
003410     SELECT W42821                     ASSIGN TO W42821D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W42821                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004010*01  POST -COPY W4282101 -PRE  UT21-  -L.                                 
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W4282100'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004800 01  W-SPAR-IDDISTR              PIC S9(5)   COMP-3 VALUE ZERO.           
004810 01  W-SPAR-IDKUNDNR             PIC S9(7)   COMP-3 VALUE ZERO.           
004820 01  W-SPAR-IDRAPPNR             PIC 9(7)    VALUE ZERO.                  
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
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  UT21-AREA-START             PIC X(24)   VALUE                        
007303                                 'UT21-AREA-START  '.                     
007304     SKIP2                                                                
007305                                                                          
007310*01  AREA -COPY W4282101     -PRE UT21-                                   
007400     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008710     88  SEGMENT-SLUT                        VALUE 'GB'.                  
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
010000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010100     SKIP3                                                                
010200 01  DLI-IO-AREA.                                                         
010300     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
010401     SKIP3                                                                
010402     03  WLRETA01 REDEFINES IO-AREA.                                      
010410*        05  -COPY WDA301  -PRE RETA-                                     
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011001     EJECT                                                                
011002*01  -COPY W0008  -PRE RETA-                                              
011010     05  FILLER                  PIC X.                                   
011100     EJECT                                                                
011201 PROCEDURE DIVISION  USING RETA-PCB.                                      
011202 MAIN SECTION.                                                            
011210     ENTRY 'DLITCBL' USING RETA-PCB.                                      
011300                                                                          
011500                                                                          
011600     PERFORM A-INIT                                                       
011700     PERFORM IMS-GN-WLRETA01                                              
011800     PERFORM UNTIL SEGMENT-SLUT                                           
012000       IF RETA-RET-IDDISTR  = W-SPAR-IDDISTR  AND                         
012100          RETA-RET-IDKUNDNR = W-SPAR-IDKUNDNR AND                         
012200          RETA-RET-IDRAPPNR = W-SPAR-IDRAPPNR                             
012300         CONTINUE                                                         
012400       ELSE                                                               
012500         PERFORM B-SKAPA-UTFIL21                                          
012520         MOVE RETA-RET-IDDISTR  TO W-SPAR-IDDISTR                         
012530         MOVE RETA-RET-IDKUNDNR TO W-SPAR-IDKUNDNR                        
012540         MOVE RETA-RET-IDRAPPNR TO W-SPAR-IDRAPPNR                        
012550       END-IF                                                             
012560       PERFORM IMS-GN-WLRETA01                                            
012600     END-PERFORM                                                          
012700                                                                          
012800                                                                          
012900     PERFORM Z-FINIT                                                      
013000                                                                          
013100     MOVE ZERO TO RETURN-CODE                                             
013200     GOBACK                                                               
013300     .                                                                    
013400     EJECT                                                                
013500 A-INIT SECTION.                                                          
013701                                                                          
013710     OPEN OUTPUT W42821                                                   
013800                                                                          
013900     ACCEPT DAGENS-DATUM  FROM DATE                                       
014010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014200     .                                                                    
014300     EJECT                                                                
014310 B-SKAPA-UTFIL21 SECTION.                                                 
014320                                                                          
014340     MOVE RETA-RET-IDRT     TO UT21-IDRT                                  
014341     MOVE RETA-RET-IDDISTR  TO UT21-IDDISTR                               
014350     MOVE RETA-RET-IDKUNDNR TO UT21-IDKUNDNR                              
014360     MOVE RETA-RET-IDRAPPNR TO UT21-IDRAPPNR                              
014370     MOVE RETA-RET-DASNDDAT (3:6) TO UT21-TISNDDAT                        
014380     MOVE RETA-RET-DAREGDAT (3:6) TO UT21-TIREGDAT                        
014390     MOVE RETA-RET-TILOSSN  TO UT21-TILOSSN                               
014391     MOVE RETA-RET-TIINLMOT TO UT21-TIINLMOT                              
014392                                                                          
014393     PERFORM S11-SKRIV-W42821                                             
014394     .                                                                    
014395     EJECT                                                                
014400 Z-FINIT SECTION.                                                         
014510     CLOSE W42821                                                         
014601     SKIP2                                                                
014602     MOVE 'S' TO POSTSUM-OPKOD                                            
014610     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014901     EJECT                                                                
014902 S11-SKRIV-W42821 SECTION.                                                
014903                                                                          
014904     WRITE UT21-POST FROM UT21-AREA                                       
014905                                                                          
014907     MOVE 'W42821' TO POSTSUM-FDNAMN                                      
014908     MOVE 'W42821D1' TO POSTSUM-DDNAMN2                                   
014909     CALL POSTSUM USING POSTSUM-PARM                                      
014910     .                                                                    
015100     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900     SKIP3                                                                
016001     EJECT                                                                
016002 IMS-GN-WLRETA01  SECTION.                                                
016003                                                                          
016004     MOVE 'WLRETA01 ' TO SSA1                                             
016006     MOVE '  GB' TO GODK-STATUSKODER                                      
016007     CALL CBLTDLI USING GN RETA-PCB DLI-IO-AREA SSA1                      
016008     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
016009     PERFORM IMS-STATUSKONTROLL                                           
016010     .                                                                    
016100     EJECT                                                                
016200 IMS-STATUSKONTROLL SECTION.                                              
016300                                                                          
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
