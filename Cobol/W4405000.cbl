000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W4405000.                                            
000300 AUTHOR.             LASSE DAHLQVIST.                                     
000400 DATE-WRITTEN.       29 OCT, 1976                                         
000500     EJECT                                                                
000600     REMARKS.                                                             
000700*                                                                         
000800*    PROGRAMMET HELT OMSKRIVET  FEBR 1991                                 
001000*    ANNELIE ENGLUND                                                      
001100*                                                                         
001200*                                                                         
001300*    FUNKTION.                                                            
001400*                                                                         
001500*    LÄSER NER WDJ2 TILL EN SEKV. FIL                                     
001600*                                                                         
001700*    UTFIL: W44050                                                        
001800*                                                                         
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400                                                                          
002500     SELECT W44050           ASSIGN TO      W44050D1.                     
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 FILE SECTION.                                                            
002900     SKIP2                                                                
003000 FD  W44050                                                               
003100     LABEL RECORD STANDARD                                                
003200     RECORDING F                                                          
003300     BLOCK CONTAINS 0.                                                    
003400*01  W44050-POST -COPY W440012    -L                                      
003600 WORKING-STORAGE SECTION.                                                 
003700     SKIP2                                                                
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  PROGRAM-NAMN                PIC X(08)   VALUE 'W4405000'.            
003900 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004000                                                                          
004010*      --- VALID IDDC CODES                                               
004020*                                                                         
004030*01    -COPY WWDCKONS                                                     
004040       EJECT                                                              
004100*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
004200                                                                          
004300 01  DYNAMISKA-SUBPROGRAM.                                                
004400   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
004500   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
004600   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
004700     EJECT                                                                
004800*    ---- PARAMETRAR TILL POSTSUM                                         
004900                                                                          
005000*01  -COPY W0005      -PRE POSTSUM-.                                      
005200                                                                          
005300*    ---- SKRIVAREA.                                                      
005400*01  AREA  -COPY W440012    -PRE UT-                                      
005600                                                                          
005700*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
005800                                                                          
005900 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
006000                                                                          
006100*    ---- STATUSKOD FRÅN IMS                                              
006200                                                                          
006300 01  STATUS-WS                   PIC XX.                                  
006400     88  SEGMENT-FINNS                      VALUE '  '.                   
006500     88  SEGMENT-SLUT                       VALUE 'GB'.                   
006600     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
006700     SKIP3                                                                
006800 01  GODK-STATUSKODER.                                                    
006900   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
007000     SKIP3                                                                
007100 01  SSA1                        PIC X(32).                               
007200     EJECT                                                                
007300*01      -COPY W0003.                                                     
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)  VALUE                         
007700                                            'DLI-IO-AREA'.                
007800 01  DLI-IO-AREA.                                                         
007900   03  IO-AREA                   PIC X(120).                              
008000*                                                                         
008100*  03  POST -COPY WDJ211  -PRE SATG- -RED IO-AREA.                        
008300     EJECT                                                                
008400 LINKAGE SECTION.                                                         
008500     SKIP2                                                                
008600*    -COPY W0008 -PRE SATG-.                                              
008800   05  FILLER                  PIC X.                                     
008900     EJECT                                                                
009000 PROCEDURE DIVISION  USING SATG-PCB.                                      
009100     ENTRY 'DLITCBL' USING SATG-PCB.                                      
009200     SKIP2                                                                
009300     PERFORM A-INIT                                                       
009400                                                                          
009500     PERFORM IMS-GN-SATG-WDJ2                                             
009600     PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS                         
009700                                                                          
009800       PERFORM S01-SKRIV-UTPOST                                           
009900       CALL POSTSUM USING POSTSUM-PARM                                    
010000       PERFORM IMS-GN-SATG-WDJ2                                           
010100                                                                          
010200     END-PERFORM                                                          
010300                                                                          
010400     PERFORM Z-FINIT                                                      
010500     MOVE ZERO TO RETURN-CODE                                             
010600     GOBACK                                                               
010700                                                                          
010800     .                                                                    
010900     EJECT                                                                
011000 A-INIT SECTION.                                                          
011100                                                                          
011200     OPEN OUTPUT W44050                                                   
011300     MOVE PROGRAM-NAMN       TO POSTSUM-PROGNAMN                          
011400     MOVE 'W44050'           TO POSTSUM-FDNAMN                            
011500     MOVE 'W44050D1'         TO POSTSUM-DDNAMN2                           
011600                                                                          
011700     .                                                                    
011800     EJECT                                                                
011900 Z-FINIT SECTION.                                                         
012000                                                                          
012100     CLOSE W44050                                                         
012200     MOVE 'S' TO POSTSUM-OPKOD                                            
012300     CALL POSTSUM USING POSTSUM-PARM                                      
012400                                                                          
012500     .                                                                    
012600     EJECT                                                                
012700 S01-SKRIV-UTPOST SECTION.                                                
012800                                                                          
012900     MOVE SATG-SRAD-IDARTNR          TO  UT-SATS-IDARTNR                  
013000     MOVE WC-CDC-SE                  TO  UT-SATS-IDDC                     
013100     MOVE SATG-SRAD-KVSATRES         TO  UT-SATS-KVSATRES                 
013200     WRITE W44050-POST FROM UT-AREA                                       
013300                                                                          
013400     .                                                                    
013500     EJECT                                                                
013600*    ---- IMS SEKTIONER                                                   
013700 IMS-GN-SATG-WDJ2 SECTION.                                                
013800                                                                          
013900     MOVE 'WLSATG11 ' TO SSA1                                             
014000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
014100     CALL CBLTDLI USING GN SATG-PCB DLI-IO-AREA SSA1                      
014200     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
014300     PERFORM IMS-STATUSKONTROLL                                           
014400     .                                                                    
014500     SKIP3                                                                
014600 IMS-STATUSKONTROLL SECTION.                                              
014700                                                                          
014800     SET STATUS-IX TO 1                                                   
014900     SEARCH GODK-STATUS                                                   
015000     AT END                                                               
015100     STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                               
015200     DELIMITED BY SIZE INTO FELTEXT                                       
015300     CALL FELLOG                                                          
015400     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
015500     END-SEARCH                                                           
015600     .                                                                    
