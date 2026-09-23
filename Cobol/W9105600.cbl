000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9105600.                                                
000400*AUTHOR.         GÖRAN KJELLSON.                                          
000500*DATE-WRITTEN.   HÖSTEN 2015                                              
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER NER WDF201-SEGMENT TILL EN FIL                             
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDF2A MED SB                               
001300                                                                          
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000*          --- UTFIL WDF2A1 POSTER                                        
002100     SELECT W91056                     ASSIGN TO W91056D1.                
002200                                                                          
002210                                                                          
002300 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600                                                                          
002700 FD  W91056                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000     SKIP2                                                                
003100 01  UTPOST -COPY WDF2A1  -L.                                             
003200                                                                          
003210                                                                          
003300 WORKING-STORAGE SECTION.                                                 
003401                                                                          
003500 77  IDPGM                       PIC X(8)    VALUE 'W9105600'.            
003510 77  JA                          PIC X(8)    VALUE 'J'.                   
003520 77  NEJ                         PIC X(8)    VALUE 'N'.                   
003530 77  FELTEXT                     PIC X(32)   VALUE SPACE.                 
003540 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
003610 01  W-DAGENS-DATUM              PIC 9(8)    VALUE 20000000.              
003620 01  FILLER REDEFINES W-DAGENS-DATUM.                                     
003630     03  FILLER                  PIC 9(2).                                
003640     03  DAGENS-DATUM            PIC 9(6).                                
003700                                                                          
003730                                                                          
003800 01  DYNAMISKA-SUBPROGRAM.                                                
003900*                                                                         
004000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004101     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004110     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
004200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
004300                                                                          
004310                                                                          
004400*    --- PARAMETRAR TILL WDATKONV                                         
004500*01  -COPY WDATAREA                                                       
004600                                                                          
004601                                                                          
004610*    --- PARAMETRAR TILL POSTSUM                                          
004620*01  -COPY W0005   -PRE POSTSUM-                                          
004630                                                                          
004640                                                                          
007200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007300*                                                                         
007400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007500                                                                          
007600*    --- STATUS-KOD FRÅN IMS                                              
007700 01  STATUS-WS                   PIC XX.                                  
007800     88  SEGMENT-FINNS                       VALUE '  '.                  
007900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008000     88  BASEN-SLUT                          VALUE 'GB'.                  
008100                                                                          
008200 01  GODK-STATUSKODER.                                                    
008300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008400     EJECT                                                                
008500*    --- IMS FUNKTIONSKODER                                               
008600*01  -COPY W0003                                                          
008700     EJECT                                                                
008800*    ---  DLI INPUT-OUTPUT AREA                                           
008900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
009000                                                                          
009100 01  DLI-IO-AREA.                                                         
009200                                                                          
009500   03  -COPY WDF2A1                                                       
009600                                                                          
009700                                                                          
010400 LINKAGE SECTION.                                                         
010500 01  -COPY W0008  -PRE WDF2-                                              
010600     05  FILLER                  PIC X.                                   
010700                                                                          
010710                                                                          
010800 PROCEDURE DIVISION  USING WDF2-PCB.                                      
010900                                                                          
011000                                                                          
011010     PERFORM A-INIT                                                       
011400                                                                          
011500     PERFORM IMS-GN-WDF2                                                  
011600     PERFORM UNTIL BASEN-SLUT OR SEGMENT-SAKNAS                           
011700                                                                          
011901       IF SEQA-DASTADAT < W-DAGENS-DATUM                                  
011912          PERFORM S01-SKRIV-W91056                                        
011913       END-IF                                                             
011914                                                                          
011950       PERFORM IMS-GN-WDF2                                                
012030     END-PERFORM                                                          
012070                                                                          
012071     PERFORM Z-FINIT                                                      
012200     MOVE ZERO TO RETURN-CODE                                             
012300     GOBACK                                                               
012400     .                                                                    
012500                                                                          
012501                                                                          
012510 A-INIT SECTION.                                                          
012511                                                                          
012520     OPEN OUTPUT W91056                                                   
012530     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
012531                                                                          
012532     MOVE 'IDAG'          TO DAT-KDDATFORM                                
012534                                                                          
012535     CALL WDATKONV  USING DAT-KDDATFORM                                   
012536                          DAT-I-TIDATUM                                   
012537                          DAT-O-TIDATUM                                   
012538                          DAT-KDSVAR                                      
012539     IF DAT-KDSVAR-FEL                                                    
012540        MOVE 'FEL FRÅN WDATKONV I A-SECTION'                              
012541                          TO FELTEXT                                      
012542        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
012543     END-IF                                                               
012544     MOVE DAT-TIAAMMDD    TO DAGENS-DATUM                                 
012546     .                                                                    
012550                                                                          
012560                                                                          
013380 Z-FINIT SECTION.                                                         
013381                                                                          
013382     CLOSE W91056                                                         
013383     MOVE 'S' TO POSTSUM-OPKOD                                            
013384     CALL POSTSUM USING POSTSUM-PARM                                      
013460     .                                                                    
013461                                                                          
013462                                                                          
013463 S01-SKRIV-W91056 SECTION.                                                
013470                                                                          
013480     WRITE UTPOST FROM SEQA-WDF2A1                                        
013490                                                                          
013500     MOVE 'W91056'   TO POSTSUM-FDNAMN                                    
013600     MOVE 'W91056D1' TO POSTSUM-DDNAMN2                                   
013700     MOVE 'WDF2'     TO POSTSUM-TRANSTYP                                  
013800     CALL POSTSUM USING POSTSUM-PARM                                      
013823     .                                                                    
013824                                                                          
013825                                                                          
013826 IMS-GN-WDF2 SECTION.                                                     
013827                                                                          
013828     CALL CBLTDLI USING GN WDF2-PCB DLI-IO-AREA                           
013829     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
013830     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
013831     PERFORM IMS-STATUSKONTROLL                                           
013840     .                                                                    
013850                                                                          
013851                                                                          
013860 IMS-STATUSKONTROLL SECTION.                                              
013870                                                                          
013880     SET STATUS-IX TO 1                                                   
013890     SEARCH GODK-STATUS                                                   
013900       AT END                                                             
014000         CALL FELLOG                                                      
014100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
014200         CONTINUE                                                         
014300     END-SEARCH                                                           
014400     .                                                                    
