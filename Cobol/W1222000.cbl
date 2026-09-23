000100 ID DIVISION.                                                             
000200 PROGRAM-ID.        W1222000.                                             
000300 AUTHOR.            P DAHLÖF.                                             
000400 DATE-WRITTEN.      AUG 1987.                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*            RENSA B-MÄRKTA ARTIKLAR                                      
000900*            FRÅN INLEVERANSHISTORIK PÅ WDL2                              
001000                                                                          
001100*    B-MÄRKNING SKER:                                                     
001200*            I PROGRAM W12204                                             
001300                                                                          
001400                                                                          
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*- - - - - - - - - - - - - - INFILER:                                     
002100     SELECT W12207         ASSIGN TO     W12220D1.                        
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500     SKIP3                                                                
002600 FD   W12207                                                              
002700      RECORDING F                                                         
002800      BLOCK CONTAINS 0.                                                   
002900*01   POST -COPY W12207   -PRE W12207-    -L.                             
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W1222200'.            
003500 01  GENERELLA-KONSTANTER.                                                
003600   03  JA                        PIC X       VALUE 'J'.                   
003700   03  NEJ                       PIC X       VALUE 'N'.                   
003800   03  W12207-EOF                PIC X       VALUE 'N'.                   
003900     SKIP2                                                                
003910 01  W-DLET-WDL201               PIC 9(7)    VALUE ZERO.                  
003920     SKIP2                                                                
004000 01  DYNAMISKA-SUBPROGRAM.                                                
004100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
004200   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
004300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
004400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
004500     EJECT                                                                
004600*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
004700*01  -COPY W0005     -PRE  POSTSUM-.                                      
004800     EJECT                                                                
004900*- - - - - - - - - - - - - -  ARBETSAREAR FÖR INFIL                       
005000 01  FILLER                      PIC X(24)    VALUE 'INFIL'.              
005100                                                                          
005200*01  AREA    -COPY W12207   -PRE W-IN-.                                   
005300     EJECT                                                                
005400*- - - - - - - - -PARAMETER-AREOR TILL IMS-SUBPGM-SEKTIONER.              
005500 01  FILLER                      PIC X(16) VALUE 'IMS-WS'.                
005600     SKIP3                                                                
005700*- - - - - - - - -STATUSKOD FRÅN IMS                                      
005800 01  STATUS-WS                   PIC X(2).                                
005900   88  SEGMENT-FINNS         VALUE '  '.                                  
006000   88  SEGMENT-SAKNAS        VALUE 'GE'.                                  
006100   88  SEGMENT-FINNS-REDAN   VALUE 'II'.                                  
006200   SKIP3                                                                  
006300 01  GODK-STATUSKODER.                                                    
006400    03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
006500 01  SSA1                        PIC X(32).                               
006600 01  SSA2                        PIC X(32).                               
006700 01  SSA3                        PIC X(32).                               
006800*- - - - - - - - - - - - - - NYCKLAR TILL DLI.                            
006900 01  NYCKLAR-TILL-DLI.                                                    
007000   03   W-IDARTNR-X.                                                      
007100     05 W-IDARTNR                PIC S9(9)  COMP-3.                       
007200     EJECT                                                                
007300*01   -COPY W0003                                                         
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)    VALUE                       
007600                                              'DLI-IO-AREA'.              
007700 01  DLI-IO-AREA.                                                         
007800   03  IO-AREA                   PIC X(130) VALUE SPACE.                  
007900*03  WLINLE01  -COPY WDL201  -RED IO-AREA.                                
008000     EJECT                                                                
008100 LINKAGE SECTION.                                                         
008200                                                                          
008300*01  -COPY W0008       -PRE INLE-.                                        
008400 05  FILLER             PIC X.                                            
008500     EJECT                                                                
008600 PROCEDURE DIVISION USING INLE-PCB.                                       
008700 MAIN SECTION.                                                            
008800     ENTRY 'DLITCBL' USING INLE-PCB.                                      
008900                                                                          
009000     PERFORM A-INIT                                                       
009100     PERFORM S11-LAES-INFIL                                               
009200     PERFORM UNTIL W12207-EOF = JA                                        
009300       IF W-IN-UTFIL-TYP = 'B'                                            
009400         MOVE W-IN-IDARTNR  TO W-IDARTNR                                  
009500         PERFORM IMS-GHU-INLE01                                           
009600         IF SEGMENT-FINNS                                                 
009700           PERFORM IMS-DELETE-INLE                                        
009800           ADD 1 TO W-DLET-WDL201                                         
009900         END-IF                                                           
010000       END-IF                                                             
010100       PERFORM S11-LAES-INFIL                                             
010200     END-PERFORM                                                          
010300                                                                          
010400     PERFORM Z-FINIT                                                      
010500     MOVE ZERO TO RETURN-CODE                                             
010600     GOBACK                                                               
010700     .                                                                    
010800     EJECT                                                                
010900 A-INIT SECTION.                                                          
011000                                                                          
011100     OPEN  INPUT W12207                                                   
011200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011300     .                                                                    
011400     EJECT                                                                
011500 S11-LAES-INFIL SECTION.                                                  
011600                                                                          
011700     READ W12207 INTO W-IN-AREA                                           
011800         AT END MOVE JA    TO W12207-EOF                                  
011900     END-READ                                                             
012000     IF W12207-EOF = NEJ                                                  
012100       MOVE 'W12220D1'   TO POSTSUM-DDNAMN2                               
012200       MOVE 'W12207'     TO POSTSUM-FDNAMN                                
012300       CALL POSTSUM USING POSTSUM-PARM                                    
012400     END-IF                                                               
012500     .                                                                    
012600     EJECT                                                                
012700 Z-FINIT SECTION.                                                         
012800                                                                          
012810     DISPLAY ' ANTAL BORTTAGNA WDL201 SEGMENT: ' W-DLET-WDL201            
012900     CLOSE W12207                                                         
013000     MOVE 'S' TO POSTSUM-OPKOD                                            
013100     CALL POSTSUM USING POSTSUM-PARM                                      
013200     .                                                                    
013300     EJECT                                                                
013400*- - - - - - - - - - - - - - - - - - - -IMS-SECTIONER.                    
013500 IMS-GHU-INLE01 SECTION.                                                  
013600     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
013700            DELIMITED BY SIZE INTO SSA1                                   
013800     MOVE '  GE'              TO GODK-STATUSKODER                         
013900     CALL CBLTDLI USING GHU   INLE-PCB DLI-IO-AREA SSA1                   
014000     MOVE INLE-STATUS-CODE  TO STATUS-WS                                  
014100     PERFORM IMS-STATUSKONTROLL                                           
014200     .                                                                    
014300     SKIP3                                                                
014400 IMS-DELETE-INLE SECTION.                                                 
014500     MOVE '  '              TO GODK-STATUSKODER                           
014600     CALL CBLTDLI USING DLET  INLE-PCB DLI-IO-AREA                        
014700     MOVE INLE-STATUS-CODE  TO STATUS-WS                                  
014800     PERFORM IMS-STATUSKONTROLL                                           
014900     .                                                                    
015000     SKIP3                                                                
015100 IMS-STATUSKONTROLL SECTION.                                              
015200     SET STATUS-IX TO 1                                                   
015300     SEARCH GODK-STATUS                                                   
015400       AT END                                                             
015500         CALL FELLOG                                                      
015600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015700         CONTINUE                                                         
015800     END-SEARCH                                                           
015900     .                                                                    
