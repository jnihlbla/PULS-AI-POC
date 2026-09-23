001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4753100.                                                
001300 AUTHOR.         GÖRAN KJELLSSON.                                         
001400 DATE-WRITTEN.   20/01/29.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        MIC WHITE LIST EXTRACT                                           
001900*                                                                         
002010*        PROGRAMMET LÄSER      WDR5                                       
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301                                                                          
003302*          --- MIC WHITE LIST EXTRACT                                     
003310     SELECT W47531                     ASSIGN TO W47531D1.                
003500                                                                          
003510                                                                          
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W47531                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  POST -COPY WDGX4254 -PRE  MIC-  -L.                                  
004000                                                                          
004010                                                                          
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4753100'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005400                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100                                                                          
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700                                                                          
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101                                                                          
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301                                                                          
007302 01  MIC-AREA-START              PIC X(24)   VALUE                        
007303                                 'MIC-AREA-START  '.                      
007304                                                                          
007305                                                                          
007310*01  AREA -COPY WDGX4254     -PRE MIC-                                    
007400                                                                          
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700                                                                          
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900                                                                          
008000 01  NYCKLAR-TILL-DLI.                                                    
008101     03  W-WDGX4253-X.                                                    
008102         05  W-IDHTYP            PIC X(4)     VALUE '4253'.               
008103         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
008104                                                                          
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008800                                                                          
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100                                                                          
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400                                                                          
009410                                                                          
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700                                                                          
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR501'.                      
010002 01  DLI-IO-WDR501.                                                       
010003*    03  -COPY WDGX01                                                     
010004                                                                          
010005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4254'.                    
010006 01  DLI-IO-WDGX4254.                                                     
010010*    03  -COPY WDGX4254                                                   
010300                                                                          
010400 LINKAGE SECTION.                                                         
010500                                                                          
010601                                                                          
010602*01  -COPY W0008  -PRE WDR5-                                              
010610     05  FILLER                  PIC X.                                   
010700                                                                          
010800                                                                          
010801 PROCEDURE DIVISION  USING WDR5-PCB.                                      
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING WDR5-PCB.                                      
010900                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011410     PERFORM IMS-GN-WDGX4254                                              
011420                                                                          
011500     PERFORM UNTIL SEGMENT-SAKNAS                                         
011600        PERFORM S11-SKRIV-W47531                                          
011610        PERFORM IMS-GN-WDGX4254                                           
011700     END-PERFORM                                                          
012500                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100                                                                          
013200 A-INIT SECTION.                                                          
013401                                                                          
013410     OPEN OUTPUT W47531                                                   
013500                                                                          
013600     ACCEPT DAGENS-DATUM  FROM DATE                                       
013710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013900     .                                                                    
014000                                                                          
014100 Z-FINIT SECTION.                                                         
014210     CLOSE W47531                                                         
014301     SKIP2                                                                
014302     MOVE 'S' TO POSTSUM-OPKOD                                            
014310     CALL POSTSUM USING POSTSUM-PARM                                      
014400     .                                                                    
014601                                                                          
014602 S11-SKRIV-W47531 SECTION.                                                
014603                                                                          
014604     WRITE MIC-POST FROM 4254-WDGX4254                                    
014605                                                                          
014606     MOVE 'MIC'      TO POSTSUM-TRANSTYP                                  
014607     MOVE 'W47531'   TO POSTSUM-FDNAMN                                    
014608     MOVE 'W47531D1' TO POSTSUM-DDNAMN2                                   
014609     CALL POSTSUM USING POSTSUM-PARM                                      
014610     .                                                                    
014800                                                                          
014900 S99-ABEND SECTION.                                                       
015000                                                                          
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015200     CALL ABEND USING RKOD-ABEND                                          
015300     .                                                                    
015400                                                                          
015410                                                                          
015500* --- IMS SEKTIONER ---                                                   
015600                                                                          
015712 IMS-GN-WDGX4254 SECTION.                                                 
015713                                                                          
015714     STRING 'WDR501  (WDGXKEY  =' W-WDGX4253-X ')'                        
015715          DELIMITED BY SIZE INTO SSA1                                     
015716     MOVE 'WDGX4254 '         TO SSA2                                     
015718     MOVE '  GE'              TO GODK-STATUSKODER                         
015719     CALL CBLTDLI USING GN WDR5-PCB DLI-IO-WDGX4254 SSA1 SSA2             
015720     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
015721     PERFORM IMS-STATUSKONTROLL                                           
015730     .                                                                    
015800                                                                          
015900 IMS-STATUSKONTROLL SECTION.                                              
016000                                                                          
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GODK-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016500           DELIMITED BY SIZE INTO FELTEXT                                 
016600         DISPLAY FELTEXT                                                  
016700         CALL FELLOG                                                      
016800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
