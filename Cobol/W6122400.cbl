001100 ID DIVISION.                                                             
001200                                                                          
001300 PROGRAM-ID.     W6122400.                                                
001400 AUTHOR.         BODIL LINDAHL.                                           
001500 DATE-WRITTEN.   97/01/09.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        LÄSER FIL W61224 OCH TAR BORT                                    
002100*        FÄRDIGBEHANDLADE HTR 6305 LEV ANM NDC                            
002200*                                                                         
002310*        PROGRAMMET UPPDATERAR WL6305 (WDGX)                              
002400*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003601                                                                          
003602*          --- HTR 6305 BORTTAG                                           
003610     SELECT W61224                     ASSIGN TO W61224D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201                                                                          
004202 FD  W61224                                                               
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205                                                                          
004210*01  -COPY W61224      -L.                                                
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004501                                                                          
004510*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W6122400'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 01  FELTEXT.                                                             
005100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005401                                                                          
005402 77  W61224-EOF-SW               PIC X       VALUE 'N'.                   
005410     88  END-OF-W61224                       VALUE 'J'.                   
005700                                                                          
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
007202 01  IN-AREA-START               PIC X(24)   VALUE                        
007203                                             'IN-AREA-START'.             
007204     SKIP2                                                                
007205                                                                          
007210*01  AREA -COPY W61224     -PRE IN-                                       
007300*                                                                         
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007600     SKIP3                                                                
007700 01  NYCKLAR-TILL-DLI.                                                    
007801     03  W-6305KEY-X.                                                     
007802         05  W-6305-IDHTYP       PIC X(4)    VALUE '6305'.                
007803         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
007804     03  W-IDFAKT-X.                                                      
007810         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
007900     SKIP2                                                                
008000*    --- STATUS-KOD FRÅN IMS                                              
008100 01  STATUS-WS                   PIC XX.                                  
008200     88  SEGMENT-FINNS                       VALUE '  '.                  
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
010001 01  FILLER         PIC X(24) VALUE 'DLI-IO-AREA'.                        
010002 01  DLI-IO-AREA.                                                         
010010*    03  -COPY WDGX6306                                                   
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010600*01  -COPY W0009   -PRE MSG-                                              
010701     EJECT                                                                
010702*01  -COPY W0008   -PRE 6305-                                             
010710     05  FILLER                  PIC X.                                   
011000     EJECT                                                                
011101 PROCEDURE DIVISION  USING MSG-PCB 6305-PCB.                              
011102 MAIN SECTION.                                                            
011110     ENTRY 'DLITCBL' USING MSG-PCB 6305-PCB.                              
011200                                                                          
011500     PERFORM A-INIT                                                       
011600                                                                          
011610     PERFORM S01-LAES-W61224                                              
011700     PERFORM UNTIL END-OF-W61224                                          
011800        PERFORM B-BEHANDLA-FAKTURA                                        
012410        PERFORM S01-LAES-W61224                                           
012500     END-PERFORM                                                          
012600                                                                          
012800     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013601                                                                          
013610     OPEN INPUT W61224                                                    
014210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014500     .                                                                    
014600     SKIP3                                                                
014610 B-BEHANDLA-FAKTURA SECTION.                                              
014620                                                                          
014630     MOVE IN-IDFAKT TO W-IDFAKT                                           
014640     PERFORM IMS-GET-WL630511                                             
014641     PERFORM IMS-DLET-WL630511                                            
014650     .                                                                    
014660     SKIP3                                                                
014700 Z-FINIT SECTION.                                                         
014800                                                                          
014910     CLOSE W61224                                                         
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015300     .                                                                    
015401     EJECT                                                                
015402 S01-LAES-W61224  SECTION.                                                
015403                                                                          
015404     READ W61224 INTO IN-AREA                                             
015405     AT END                                                               
015407        SET END-OF-W61224 TO TRUE                                         
015408                                                                          
015409     NOT AT END                                                           
015410        MOVE 'W61224'   TO POSTSUM-FDNAMN                                 
015411        MOVE 'W61224D1' TO POSTSUM-DDNAMN2                                
015412        MOVE SPACE      TO POSTSUM-TRANSTYP                               
015413        CALL POSTSUM USING POSTSUM-PARM                                   
015414     END-READ                                                             
015420     .                                                                    
015700     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900     SKIP3                                                                
016002 IMS-GET-WL630511 SECTION.                                                
016003                                                                          
016004     STRING 'WL630501(WDGXKEY  =' W-6305KEY-X ')'                         
016005          DELIMITED BY SIZE INTO SSA1                                     
016006     STRING 'WL630511(IDFAKT   =' W-IDFAKT-X ')'                          
016007          DELIMITED BY SIZE INTO SSA2                                     
016008     MOVE '  GE' TO GODK-STATUSKODER                                      
016009     CALL CBLTDLI USING GHU 6305-PCB DLI-IO-AREA SSA1 SSA2                
016010     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
016011     PERFORM IMS-STATUSKONTROLL                                           
016012     .                                                                    
016013     SKIP3                                                                
016024 IMS-DLET-WL630511 SECTION.                                               
016025                                                                          
016026     MOVE '  ' TO GODK-STATUSKODER                                        
016027     CALL CBLTDLI USING DLET 6305-PCB DLI-IO-AREA                         
016028     MOVE 6305-STATUS-CODE TO STATUS-WS                                   
016029     PERFORM IMS-STATUSKONTROLL                                           
016030     .                                                                    
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
