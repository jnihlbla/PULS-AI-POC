000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W2615A00.                                                
000301 AUTHOR.         UMESH JAIN.                                              
000402 DATE-WRITTEN.   JULY 2011.                                               
000501*        PROGRAMMET ÄR ETT SB-PROGRAM SOM LÄSER WDK7 OCH                  
000600*        SKRIVER UT ARTKLAR MED TISKROT-AUTO > DAGENS-DATUM               
000700     EJECT                                                                
000800 ENVIRONMENT DIVISION.                                                    
000900 INPUT-OUTPUT SECTION.                                                    
001000 FILE-CONTROL.                                                            
001100     SKIP2                                                                
001200*---------------------------------------                                  
001300*                                        OUTPUT                           
001400                                                                          
001501         SELECT UTFIL          ASSIGN TO UT-S-W2615AD1.                   
001600                                                                          
001700     SKIP3                                                                
001800 DATA DIVISION.                                                           
001900 FILE SECTION.                                                            
002000 FD  UTFIL                                                                
002100     RECORDING F                                                          
002200     BLOCK 0                                                              
002300     LABEL RECORD STANDARD.                                               
002400                                                                          
002501*01  POST    -COPY W2615B -PRE UT- -L.                                    
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800     SKIP3                                                                
002900                                                                          
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  JA                      PIC X        VALUE 'J'.                      
003300 77  NEJ                     PIC X        VALUE 'N'.                      
003400                                                                          
003500 01  W-ANTAL-POST            PIC S9(7)    COMP-3 VALUE ZERO.              
003600 01  WS-DAGENS-DATUM         PIC S9(7)    COMP-3 VALUE ZERO.              
003700     SKIP3                                                                
003800 01  DYNAMISKA-SUBPROGRAM.                                                
003900  03 POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.                 
004000  03 CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.                 
004200  03 FELLOG                  PIC X(8)    VALUE 'FELLOG '.                 
004300  03 WDATKONV                PIC X(8)    VALUE 'WDATKONV'.                
004400     EJECT                                                                
004500 01  FILLER  PIC X(16)  VALUE 'POSTSUM'.                                  
004600                                                                          
004700*01          -COPY W0005    -PRE POSTSUM-                                 
004800     EJECT                                                                
004900 01  FILLER  PIC X(16)  VALUE 'WDATKONV'.                                 
005000                                                                          
005100*01          -COPY WDATAREA                                               
005200     EJECT                                                                
005300 01  FILLER  PIC X(32)  VALUE 'AREA FÖR POST PÅ UTFIL'.                   
005400                                                                          
005500*01  AREA    -COPY W2615B   -PRE UT-                                      
005600     EJECT                                                                
005700 01  FILLER                  PIC X(8)  VALUE 'IMS-WS  '.                  
005800     SKIP3                                                                
005900 01  NYCKLAR-TILL-DLI.                                                    
006000     03  W-IDARTNR-X.                                                     
006100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
006200                                                                          
006501     03  W-IDSKYLT-X.                                                     
006601         05  W-IDSKYLT           PIC  X(3)   VALUE 'S  ' .                
006701                                                                          
006800 01  STATUS-WS               PIC X(2).                                    
006900     88  SEGMENT-FINNS                 VALUE '  '.                        
007000     88  SEGMENT-SLUT                  VALUE 'GB'.                        
007100     88  SEGMENT-SAKNAS                VALUE 'GE'.                        
007200                                                                          
007300 01  GODK-STATUSKODER.                                                    
007400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
007500     SKIP3                                                                
007600 01  SSA1                        PIC X(64).                               
007700 01  SSA2                        PIC X(64).                               
007800                                                                          
007900     EJECT                                                                
008000*01  -COPY W0003                                                          
008100     EJECT                                                                
008200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
008300                                                                          
008400 01  DLI-IO-AREA             PIC X(800).                                  
008500     SKIP3                                                                
008601*01  SEGM01      -COPY WDK701 -RED DLI-IO-AREA                            
008700     EJECT                                                                
008801*01  SEGM30      -COPY WDK711 -RED DLI-IO-AREA                            
008900     EJECT                                                                
009001 01  DLI-IO-WDD311.                                                       
009101*    03  -COPY WDD311   -PRE WDD3-                                        
009201     EJECT                                                                
009300 LINKAGE SECTION.                                                         
009401*01  -COPY W0008   -PRE WDK7-                                             
009500    05 FILLER         PIC X.                                              
009601*01  -COPY W0008   -PRE WDD3-                                             
009701    05 FILLER         PIC X.                                              
009801     EJECT                                                                
009901 PROCEDURE DIVISION   USING WDK7-PCB WDD3-PCB.                            
010001     ENTRY 'DLITCBL'  USING WDK7-PCB WDD3-PCB.                            
010101                                                                          
010201     PERFORM A-INITIERA                                                   
010301                                                                          
010401     PERFORM IMS-GET-WDK7                                                 
010501     PERFORM UNTIL SEGMENT-SLUT                                           
010601*****OR W-ANTAL-POST > 1000                                               
010701        EVALUATE WDK7-SEG-NAME-FB                                         
010801           WHEN  'WDK701  '                                               
010901                 PERFORM B-FLYTTA-WDK701                                  
011001           WHEN  'WDK711  '                                               
011101                 IF SLAG-TISKROT-AUTO NUMERIC  AND                        
011201                    SLAG-TISKROT-AUTO > WS-DAGENS-DATUM                   
011301                   PERFORM D-FLYTTA-WDK711                                
011401                   PERFORM S10-SKAPA-UTPOST                               
011501                 END-IF                                                   
011601        END-EVALUATE                                                      
011701        PERFORM IMS-GET-WDK7                                              
011801     END-PERFORM                                                          
011901     PERFORM Z-FINIT                                                      
012001     MOVE ZERO TO RETURN-CODE                                             
012101     GOBACK                                                               
012201     .                                                                    
012301     EJECT                                                                
012401 A-INITIERA SECTION.                                                      
012501                                                                          
012601     OPEN OUTPUT UTFIL                                                    
012701     MOVE 'W2615A'           TO POSTSUM-PROGNAMN                          
012801     PERFORM S02-NOLLSTALL-UTAREA                                         
012901     MOVE 'IDAG'             TO DAT-KDDATFORM                             
013001     CALL WDATKONV USING DAT-KDDATFORM                                    
013101                         DAT-I-TIDATUM                                    
013201                         DAT-O-TIDATUM                                    
013301                         DAT-KDSVAR                                       
013401     IF DAT-KDSVAR-OK                                                     
013501       MOVE DAT-TIAAMMDD     TO WS-DAGENS-DATUM                           
013601     ELSE                                                                 
013701       MOVE ZERO             TO WS-DAGENS-DATUM                           
013801     END-IF                                                               
013901     .                                                                    
014001     EJECT                                                                
014101 B-FLYTTA-WDK701 SECTION.                                                 
014201     SKIP2                                                                
014301     MOVE SART-IDARTNR        TO UT-IDARTNR                               
014402                                 W-IDARTNR                                
014501*                                                                         
014601*    GET DESCRIPTION OF THE PART                                          
014701     PERFORM IMS-GU-WDD311-BSEQ                                           
014801     IF SEGMENT-FINNS                                                     
014901       MOVE WDD3-TEXT-BEART TO UT-BEART                                   
015001     ELSE                                                                 
015101       MOVE SPACE           TO UT-BEART                                   
015201     END-IF                                                               
015301     .                                                                    
015401     SKIP3                                                                
015501 D-FLYTTA-WDK711 SECTION.                                                 
015601     SKIP2                                                                
015701     MOVE SLAG-IDDC          TO  UT-IDDC                                  
015801     MOVE SLAG-TISKROT-AUTO  TO  UT-TISKROT-AUTO                          
015901     .                                                                    
016001     EJECT                                                                
016101 S01-SKRIV-POST SECTION.                                                  
016201     WRITE UT-POST FROM UT-AREA                                           
016301     MOVE 'UTFIL'    TO POSTSUM-FDNAMN                                    
016401     MOVE 'W2615AD1' TO POSTSUM-DDNAMN2                                   
016501     MOVE 'POST'     TO POSTSUM-TRANSTYP                                  
016601     CALL POSTSUM USING POSTSUM-PARM                                      
016701     ADD +1          TO W-ANTAL-POST                                      
016801     .                                                                    
016901     EJECT                                                                
017001 S02-NOLLSTALL-UTAREA SECTION.                                            
017101     MOVE +0 TO UT-IDARTNR                                                
017501                UT-TISKROT-AUTO                                           
017502     MOVE SPACE TO UT-IDDC                                                
017503                   UT-BEART                                               
017601     .                                                                    
017701     EJECT                                                                
017801 S10-SKAPA-UTPOST SECTION.                                                
017901     PERFORM S01-SKRIV-POST                                               
018001     .                                                                    
018101     EJECT                                                                
018201 Z-FINIT SECTION.                                                         
018301                                                                          
018401     CLOSE UTFIL                                                          
018501     MOVE 'S' TO POSTSUM-OPKOD                                            
018601     CALL POSTSUM USING POSTSUM-PARM                                      
018701     .                                                                    
018801     EJECT                                                                
018901 IMS-GET-WDK7 SECTION.                                                    
019001     SKIP2                                                                
019101     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
019201     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA                           
019301     MOVE WDK7-STATUS-CODE   TO STATUS-WS                                 
019401     PERFORM IMS-STATUSKONTROLL                                           
019501     SKIP2                                                                
019601     .                                                                    
019701 IMS-GU-WDD311-BSEQ SECTION.                                              
019802     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
019901          DELIMITED BY SIZE INTO SSA1                                     
020001     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
020101          DELIMITED BY SIZE INTO SSA2                                     
020201     MOVE '  GE' TO GODK-STATUSKODER                                      
020301     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
020401     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
020501     PERFORM IMS-STATUSKONTROLL                                           
020601     .                                                                    
020701     EJECT                                                                
020800 IMS-STATUSKONTROLL SECTION.                                              
020900     SET STATUS-IX  TO 1                                                  
021000     SEARCH GODK-STATUS   AT END CALL FELLOG                              
021100     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
021200     CONTINUE                                                             
022000     END-SEARCH                                                           
030000     .                                                                    
