000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6133000.                                                
000400*AUTHOR.         UMESH JAIN                                               
000500*DATE-WRITTEN.   13/05/16.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        READ WDT3 AND EXTRACT KDFORP FOR CHINA                           
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDT3 MED SB                                
001300                                                                          
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000*          --- UTFIL WITH KDFORP FOR CHINESE PART NUMBERS                 
002100     SELECT W61330                     ASSIGN TO W61330D1.                
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400                                                                          
002500 FILE SECTION.                                                            
002600                                                                          
002700 FD  W61330                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000     SKIP2                                                                
003100 01  UTPOST -COPY W6133001  -L.                                           
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400     SKIP2                                                                
003401                                                                          
003402*    -- CHECKED BY WY2000                                                 
003403 77  IDPGM                       PIC X(8)    VALUE 'W6133000'.            
003404 77  FELTEXT                     PIC X(32)   VALUE SPACE.                 
003405 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
003406 77  W-PREV-IDARTNR              PIC S9(9)   VALUE 0 COMP-3.              
003407                                                                          
003408 01  DYNAMISKA-SUBPROGRAM.                                                
003409*                                                                         
003410     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
003430     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
003440     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
003450     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
003460     EJECT                                                                
003470*    --- PARAMETRAR TILL POSTSUM                                          
003480*01  -COPY W0005   -PRE POSTSUM-                                          
003490     EJECT                                                                
003500 01  FILLER                      PIC X(16)   VALUE 'UT-AREA'.             
003600 01  AREA -COPY W6133001    -PRE UT-                                      
003700     EJECT                                                                
003800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
003900*                                                                         
004000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
004100                                                                          
004200*    --- STATUS-KOD FRÅN IMS                                              
004300 01  STATUS-WS                   PIC XX.                                  
004400     88  SEGMENT-FINNS                       VALUE '  '.                  
004500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
004600     88  BASEN-SLUT                          VALUE 'GB'.                  
004700     SKIP2                                                                
004800 01  GODK-STATUSKODER.                                                    
004900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
005000     EJECT                                                                
005100*    --- IMS FUNKTIONSKODER                                               
005200*01  -COPY W0003                                                          
005300     EJECT                                                                
005400*    ---  DLI INPUT-OUTPUT AREA                                           
005500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
005600                                                                          
005700 01  DLI-IO-AREA.                                                         
005800   03  IO-AREA                   PIC X(210).                              
005900   SKIP3                                                                  
006000   03  -COPY WDT301     -RED IO-AREA                                      
006100     EJECT                                                                
006200   03  -COPY WDT311     -RED IO-AREA                                      
006300     EJECT                                                                
006400 LINKAGE SECTION.                                                         
006500 01  -COPY W0008  -PRE WDT3-                                              
006600     05  FILLER                  PIC X.                                   
006700     EJECT                                                                
006800 PROCEDURE DIVISION  USING WDT3-PCB.                                      
006900     ENTRY 'DLITCBL' USING WDT3-PCB.                                      
007000                                                                          
007100     PERFORM A-INIT                                                       
007200                                                                          
007300     PERFORM IMS-GN-WDT3                                                  
007400     PERFORM UNTIL BASEN-SLUT                                             
007500       EVALUATE WDT3-SEG-NAME-FB                                          
007600         WHEN 'WDT301'                                                    
007700           MOVE FART-IDARTNR TO UT-IDARTNR                                
007800         WHEN 'WDT311'                                                    
007900           IF W-PREV-IDARTNR = UT-IDARTNR                                 
008000             CONTINUE                                                     
008100           ELSE                                                           
008200             IF FPCK-IDLANDX2 = 'CN'                                      
008300               MOVE FPCK-KDFORP TO UT-KDFORP                              
008400               MOVE UT-IDARTNR  TO W-PREV-IDARTNR                         
008500               PERFORM S01-SKRIV-W61330                                   
008600             END-IF                                                       
008700           END-IF                                                         
008800       END-EVALUATE                                                       
008900       PERFORM IMS-GN-WDT3                                                
009000     END-PERFORM                                                          
009100                                                                          
009200     CLOSE W61330                                                         
009300     MOVE 'S' TO POSTSUM-OPKOD                                            
009400     CALL POSTSUM USING POSTSUM-PARM                                      
009500     MOVE ZERO TO RETURN-CODE                                             
009600     GOBACK                                                               
009700     .                                                                    
009800     EJECT                                                                
009900 A-INIT SECTION.                                                          
010000     OPEN OUTPUT W61330                                                   
010100     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
010200     .                                                                    
010300     EJECT                                                                
010400 S01-SKRIV-W61330 SECTION.                                                
010500     WRITE UTPOST FROM UT-AREA                                            
010600     MOVE 'W61330'   TO POSTSUM-FDNAMN                                    
010700     MOVE 'W61330D1' TO POSTSUM-DDNAMN2                                   
010800     MOVE '   '      TO POSTSUM-TRANSTYP                                  
010900     CALL POSTSUM USING POSTSUM-PARM                                      
011000     .                                                                    
011100     EJECT                                                                
011200 IMS-GN-WDT3 SECTION.                                                     
011300     CALL CBLTDLI USING GN WDT3-PCB DLI-IO-AREA                           
011400     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
011500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
011600     PERFORM IMS-STATUSKONTROLL                                           
011700     .                                                                    
011800     SKIP3                                                                
011900 IMS-STATUSKONTROLL SECTION.                                              
012000     SET STATUS-IX TO 1                                                   
012100     SEARCH GODK-STATUS                                                   
012200       AT END                                                             
012300         CALL FELLOG                                                      
012400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
012500         CONTINUE                                                         
012600     END-SEARCH                                                           
012700     .                                                                    
