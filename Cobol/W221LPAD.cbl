000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W221LPAD.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   15/01/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        SUBPOGRAM TO SORT KDLPORS IN KDLPORS PRIORITY.                   
001000*        KDLPORS PRIORITY IS UPDATED IN COPYBOOK W221LP01                 
001100*        FOR CDC AND W221LP02 FOR CHINA.                                  
001110*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 DATA DIVISION.                                                           
002100                                                                          
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400 77  IDPGM                       PIC X(8)    VALUE 'W221LPAD'.            
002500 77  CURRENT-SECTION             PIC X(40)   VALUE SPACE.                 
002600 77  YES                         PIC X       VALUE 'J'.                   
002700 77  NOO                         PIC X       VALUE 'N'.                   
002800                                                                          
002900 77  IX1                         PIC S9(4)  VALUE +0    COMP SYNC.        
002910 77  LINK-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
002920 77  LINK-IX-MAX                 PIC S9(4)  VALUE +4    COMP SYNC.        
003000 77  LP-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
003001 77  LP-IX-MAX                   PIC S9(4)  VALUE +100  COMP SYNC.        
003010 77  WT-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
003100 77  MAX-WT-IX                   PIC S9(4)  VALUE +0    COMP SYNC.        
003200                                                                          
003900 01  WT-ENTRY-PARM.                                                       
004000     03 STEGLANGD                PIC S9(9) COMP.                          
004100     03 ANTAL                    PIC S9(9) COMP.                          
004200     03 NYCKELLANGD              PIC S9(9) COMP.                          
004300                                                                          
004400 01  WT-TAB-MAX                  PIC S9(9) COMP.                          
004500                                                                          
004600 01  WT-TAB.                                                              
004700     03 WT-TABELL OCCURS 4.                                               
004800        05 WT-KDLPORS-PRIO       PIC 9(03).                               
004900        05 WT-KDLPORS            PIC 9(03).                               
005000                                                                          
005100     EJECT                                                                
005200*01  -COPY W221LP01 -PRE LP01-                                            
005300     EJECT                                                                
005310*01  -COPY W221LP02 -PRE LP02-                                            
005320     EJECT                                                                
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
005800     SKIP2                                                                
005900*    --- PARAMETERS TO ABEND                                              
006000                                                                          
006100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006400     SKIP2                                                                
006500 01  ERROR-TEXT.                                                          
006600     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
006700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006800     EJECT                                                                
006900 LINKAGE  SECTION.                                                        
007000                                                                          
007100 01  LINK-TABLE-CTXT             PIC X(08).                               
007110                                                                          
007120 01  LINK-TABLE.                                                          
007200     05 LINK-KDLPORS             PIC 9(03) OCCURS 4.                      
007810                                                                          
007900 PROCEDURE DIVISION USING LINK-TABLE-CTXT LINK-TABLE.                     
008000 MAIN SECTION.                                                            
008100                                                                          
008700     PERFORM B-FIND-KDLPORS-PRIO                                          
008900                                                                          
009000     PERFORM C-SORT-KDLPORS-PRIO                                          
009100                                                                          
009200     PERFORM D-WRITE-SORT-KDLPORS-PRIO                                    
009300                                                                          
009400     MOVE ZERO TO RETURN-CODE                                             
009500     GOBACK                                                               
009600     .                                                                    
009700     EJECT                                                                
010710*                                                                         
010800 B-FIND-KDLPORS-PRIO SECTION.                                             
010900     MOVE 'B-FIND-KDLPORS-PRIO'        TO CURRENT-SECTION                 
011000                                                                          
011001     PERFORM BA-INITIATE-WT-TAB                                           
011002                                                                          
011003     IF LINK-TABLE-CTXT = 'W221LP01'                                      
011004        PERFORM BB-W221LP01-PRIO                                          
011005     ELSE                                                                 
011006        IF LINK-TABLE-CTXT = 'W221LP02'                                   
011007           PERFORM BC-W221LP02-PRIO                                       
011008        END-IF                                                            
011009     END-IF                                                               
011010     .                                                                    
011011     EJECT                                                                
011012 BA-INITIATE-WT-TAB SECTION.                                              
011013     MOVE 'BA-INITIATE-WT-TAB'        TO CURRENT-SECTION                  
011014                                                                          
011015     MOVE +0                          TO WT-IX                            
011016     PERFORM UNTIL WT-IX = LINK-IX-MAX                                    
011017       ADD +1                         TO WT-IX                            
011018       MOVE ZERO                      TO WT-KDLPORS     (WT-IX)           
011019       MOVE ZERO                      TO WT-KDLPORS-PRIO(WT-IX)           
011020     END-PERFORM                                                          
011021     .                                                                    
011022     EJECT                                                                
011023 BB-W221LP01-PRIO SECTION.                                                
011024     MOVE 'BB-W221LP01-PRIO'            TO CURRENT-SECTION                
011025                                                                          
011026     MOVE +0                            TO LINK-IX                        
011027     MOVE +0                            TO WT-IX                          
011030     PERFORM UNTIL LINK-IX = LINK-IX-MAX                                  
011040      ADD +1                            TO LINK-IX                        
011052      MOVE +0                           TO LP-IX                          
011053      PERFORM UNTIL LP-IX = LP-IX-MAX                                     
011060        ADD +1                          TO LP-IX                          
011310        IF LINK-KDLPORS (LINK-IX) = LP01-KDLPORS (LP-IX)                  
011400          ADD +1                        TO WT-IX                          
011500          MOVE LP01-KDLPORS     (LP-IX) TO WT-KDLPORS     (WT-IX)         
011600          MOVE LP01-KDLPORS-PRIO(LP-IX) TO WT-KDLPORS-PRIO(WT-IX)         
011610          MOVE +100                     TO LP-IX                          
011613        END-IF                                                            
011620      END-PERFORM                                                         
011630     END-PERFORM                                                          
011800     .                                                                    
011801                                                                          
011802 BC-W221LP02-PRIO SECTION.                                                
011803     MOVE 'BB-W221LP02-PRIO'            TO CURRENT-SECTION                
011804                                                                          
011805     MOVE +0                            TO LINK-IX                        
011806     MOVE +0                            TO WT-IX                          
011807     PERFORM UNTIL LINK-IX = LINK-IX-MAX                                  
011808      ADD +1                            TO LINK-IX                        
011809      MOVE +0                           TO LP-IX                          
011810      PERFORM UNTIL LP-IX = LP-IX-MAX                                     
011813        ADD +1                          TO LP-IX                          
011814        IF LINK-KDLPORS (LINK-IX) = LP02-KDLPORS (LP-IX)                  
011815          ADD +1                        TO WT-IX                          
011816          MOVE LP02-KDLPORS     (LP-IX) TO WT-KDLPORS     (WT-IX)         
011817          MOVE LP02-KDLPORS-PRIO(LP-IX) TO WT-KDLPORS-PRIO(WT-IX)         
011818          MOVE +100                     TO LP-IX                          
011819        END-IF                                                            
011820      END-PERFORM                                                         
011821     END-PERFORM                                                          
011822     .                                                                    
011823                                                                          
012000 C-SORT-KDLPORS-PRIO SECTION.                                             
012100     MOVE 'C-WRITE-SORT-KDLPORS-PRIO'  TO CURRENT-SECTION                 
012200                                                                          
012500     MOVE +6                           TO STEGLANGD                       
012600     MOVE +3                           TO NYCKELLANGD                     
012700     COMPUTE ANTAL = WT-IX                                                
012800     COMPUTE MAX-WT-IX = WT-IX                                            
012900                                                                          
013000     IF WT-IX > 1                                                         
013100       CALL WINTSOR USING WT-TAB                                          
013200                          STEGLANGD                                       
013300                          ANTAL                                           
013400                          WT-KDLPORS-PRIO(1)                              
013500                          NYCKELLANGD                                     
013600     END-IF                                                               
013700     .                                                                    
013800     EJECT                                                                
013900 D-WRITE-SORT-KDLPORS-PRIO SECTION.                                       
014000     MOVE 'D-WRITE-SORT-KDLPORS-PRIO'  TO CURRENT-SECTION                 
014100                                                                          
014200*----SORT INTERN-TABLE 1. KDLPORS-PRIO                                    
014300                                                                          
014400     MOVE +0                        TO WT-IX                              
014410     MOVE +0                        TO IX1                                
014500     PERFORM UNTIL WT-IX = LINK-IX-MAX                                    
014600       ADD +1                        TO WT-IX                             
014610       ADD +1                        TO IX1                               
014700       MOVE WT-KDLPORS     (WT-IX)   TO LINK-KDLPORS(IX1)                 
014900     END-PERFORM                                                          
015000     .                                                                    
