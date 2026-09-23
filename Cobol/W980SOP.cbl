000010*COMPOPT STDSUB=YES ISPFPGM=YES                                           
000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W980SOP.                                                  
000400 AUTHOR.        KJELL ANDRE.                                              
000500     DATE-WRITTEN.  MARS 1986.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*    SUBRUTIN-INTERFACE TILL SOP. PROGRAMMET UTNYTTJAR                    
001100*    W9802000 FÖR ALL BEARBETNING, OCH EXISTERAR ENBART                   
001200*    FÖR ATT FÅ ETT INTERFACE SOM ÄR REUSEABLE.                           
001800*                                                                         
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP3                                                                
002200 DATA DIVISION.                                                           
002300     SKIP2                                                                
002400 WORKING-STORAGE SECTION.                                                 
002401*    -- CHECKED BY WY2000                                                 
002410     SKIP3                                                                
002500*                                                                         
002600 77  PROGRAM-NAMN               PIC X(8)   VALUE 'W980SOP'.               
002700*                                                                         
002710 77  JA                         PIC X      VALUE 'J'.                     
002711 77  NEJ                        PIC X      VALUE 'N'.                     
002720*                                                                         
002800 01  DYNAMISKA-SUBPROGRAM.                                                
002900     03  W9802000               PIC X(8)   VALUE 'W9802000'.              
002910     03  VIMSREGT               PIC X(8)   VALUE 'VIMSREGT'.              
003000*                                                                         
003100 01  SPAR-KDSOPFUNK             PIC X.                                    
003110*                                                                         
003120 01  FILLER.                                                              
003130     03  IMS-VIMSREGT           PIC S9(9)  COMP SYNC.                     
003140*                                                                         
003150       88  MPP                             VALUE +4.                      
003160       88  BMP                             VALUE +8.                      
003170       88  BATCH                           VALUE +16 THRU +64.            
003200     EJECT                                                                
003300*01  -COPY W98020                                                         
003500     EJECT                                                                
003600 LINKAGE SECTION.                                                         
003700                                                                          
003800 01  LINK-PARM.                                                           
003900*03  -COPY WSOPAREA   -PRE L                                              
004100     EJECT                                                                
004200 PROCEDURE DIVISION USING LINK-PARM.                                      
004500                                                                          
004600     MOVE LSOP-SOPFUNC TO SPAR-KDSOPFUNK                                  
004700     PERFORM A-LSOP-TILL-SOP20                                            
004800                                                                          
004900* FUNK 'OCAPSEHRIZKV'                                                     
005000     IF SPAR-KDSOPFUNK = 'O' OR 'C' OR 'A' OR 'P' OR 'S' OR 'E'           
005100        OR 'H' OR 'R' OR 'I' OR 'Z' OR 'K' OR 'V' OR 'F'                  
005200       MOVE LSOP-SOPFUNC TO SPAR-KDSOPFUNK                                
005300                                                                          
005400       MOVE '0' TO SOP20-KDSOPFUNK                                        
005500       CALL W9802000 USING SOP20-W98020                                   
005600                                                                          
005700       MOVE SPAR-KDSOPFUNK TO SOP20-KDSOPFUNK                             
005800       CALL W9802000 USING SOP20-W98020                                   
005900       PERFORM B-TILL-LSOP                                                
006000                                                                          
006100       MOVE '9' TO SOP20-KDSOPFUNK                                        
006200       CALL W9802000 USING SOP20-W98020                                   
006300     ELSE                                                                 
006400       MOVE 12 TO LSOP-RETCODE                                            
006500       MOVE 2  TO LSOP-MSGCODE                                            
006600     END-IF                                                               
006700                                                                          
006800     MOVE ZERO TO RETURN-CODE                                             
006900     GOBACK                                                               
007000     .                                                                    
007100     EJECT                                                                
007200 A-LSOP-TILL-SOP20  SECTION.                                              
007300     SKIP2                                                                
007400     MOVE LOW-VALUE TO SOP20-W98020                                       
007500     MOVE LSOP-DDPREFIX            TO SOP20-IDDDPREFIX                    
007600     MOVE LSOP-SOPFUNC             TO SOP20-KDSOPFUNK                     
007700     MOVE LSOP-PROC-NAME           TO SOP20-IDPROCESS                     
007800     IF LSOP-ACTPASS-DATE IS NUMERIC                                      
007900       MOVE LSOP-ACTPASS-DATE        TO SOP20-TIAPDAT                     
008000     END-IF                                                               
008100     IF LSOP-SOPFUNC = 'O' OR 'C' OR 'K'                                  
008200       MOVE LSOP-CATALOG           TO SOP20-TECATALOG                     
008300     ELSE                                                                 
008400       MOVE SPACE                  TO SOP20-TECATALOG                     
008500     END-IF                                                               
008600     IF LSOP-SOPFUNC = 'A' OR 'O' OR 'V'                                  
008700       IF LSOP-SYMBOLIC-VARIABLES NOT = LOW-VALUE                         
008800         MOVE LSOP-SYMBOLIC-VARIABLES  TO SOP20-TESYMBV                   
008900       ELSE                                                               
009000         MOVE SPACE TO SOP20-TESYMBV                                      
009100       END-IF                                                             
009200     ELSE                                                                 
009300       MOVE SPACE                  TO SOP20-TESYMBV                       
009400     END-IF                                                               
009410                                                                          
009420     CALL VIMSREGT                                                        
009430     MOVE RETURN-CODE TO IMS-VIMSREGT                                     
009440     IF MPP                                                               
009450       MOVE NEJ TO SOP20-FLBATCH                                          
009460     ELSE                                                                 
009470       MOVE JA TO SOP20-FLBATCH                                           
009480     END-IF                                                               
009500     .                                                                    
009600     EJECT                                                                
009700 B-TILL-LSOP        SECTION.                                              
009800     SKIP2                                                                
009900     MOVE SOP20-KDRET              TO LSOP-RETCODE                        
010000     MOVE SOP20-KDMEDD             TO LSOP-MSGCODE                        
010100     MOVE SOP20-KDPROCSTRT         TO LSOP-START-TYPE                     
010200     MOVE SOP20-KDPROCSTAT         TO LSOP-PROC-STATUS                    
010300     MOVE SOP20-TECATALOG          TO LSOP-CATALOG                        
010400     MOVE SOP20-TEPRED             TO LSOP-PREDECESSORS                   
010500     MOVE SOP20-TESUCC             TO LSOP-SUCCESSORS                     
010600     MOVE SOP20-TIAPDAT-SENAST     TO LSOP-LAST-ACTPASS-DATE              
010700     MOVE SOP20-TIEXDAT-SENAST     TO LSOP-LAST-EXEC-DATE                 
010800     MOVE SOP20-TIMINUT-START      TO LSOP-LAST-START-TIME                
010900     MOVE SOP20-TIMINUT-STOPP      TO LSOP-LAST-END-TIME                  
011000     MOVE SOP20-TIEXEC-SENAST      TO LSOP-LAST-EXEC-TIME                 
011100     MOVE SOP20-TIEXEC-MEDEL       TO LSOP-MEAN-EXEC-TIME                 
011200     MOVE SOP20-TEACTQ             TO LSOP-ACT-QUEUE                      
011300     MOVE SOP20-TEPASSQ            TO LSOP-PASS-QUEUE                     
011400     MOVE SOP20-FLHOLD             TO LSOP-PROC-HOLD                      
011500     MOVE SOP20-TEATTN             TO LSOP-ATTN-TEXT                      
011600     MOVE SOP20-IDPROCESS-PARENT   TO LSOP-PARENT-PROC-NAME               
011700     MOVE SOP20-KDPROCMTYP         TO LSOP-PROC-TYPE                      
011800     MOVE SOP20-KDPROCPRIO         TO LSOP-PROC-PRIO                      
011900     MOVE SOP20-KDVOUT             TO LSOP-VD-OUTPUT                      
012000     MOVE SOP20-TESYMBV            TO LSOP-SYMBOLIC-VARIABLES             
012100     .                                                                    
012200     EJECT                                                                
