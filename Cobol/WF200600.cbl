000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF200600.                                                
001300 AUTHOR.         BO HAMMARIN.                                             
001400 DATE-WRITTEN.   APR 2003.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   COMPLETES THE DOCUMENT HEADER WITH                                    
002000*   - VAT TOTALS                                                          
002100*   - BTO TOTALS                                                          
002410*                                                                         
002423*   PGM UPDATES                                                           
002424*   - ROWS IN TABLE T01DHEA                                               
002425*                                                                         
002430*   PGM READS                                                             
002440*   - ROWS IN TABLE T01PROC                                               
002460*   - ROWS IN TABLE T01DAPP                                               
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003011 FILE-CONTROL.                                                            
003012                                                                          
003013 DATA DIVISION.                                                           
003014                                                                          
003015 FILE SECTION.                                                            
003016                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                        PIC X(8)   VALUE 'WF200600'.            
004201 77  WS-DAGENS-DATUM              PIC X(8)   VALUE '00000000'.            
004202 77  WS-DAGENS-DATUM-NUM          PIC 9(8).                               
004210                                                                          
004420 01  WS-MISCELLANEOUS.                                                    
004430     03  WS-SUBTO-TOT             PIC S9(11)V9(02) COMP-3.                
004431     03  WS-SUVAT-BILLIT-TOT      PIC S9(11)V9(02) COMP-3.                
004432     EJECT                                                                
004440                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006300     03  ABEND                    PIC X(8)   VALUE 'ABEND   '.            
006310                                                                          
006400 01  RKOD-ABEND-DB2               PIC S9(4)  VALUE +998 COMP SYNC.        
006501     EJECT                                                                
006550*                                                                         
010602*        WORK-AREAS FOR DB2-SECTIONS                                      
010603*                                                                         
010604 01  FILLER                       PIC X(16)  VALUE 'PROC-TAB   '.         
010605*01  -COPY T01PROC    -PRE PROC-                                          
010606                                                                          
010607 01  FILLER                       PIC X(16)  VALUE 'DHEA-TAB   '.         
010608*01  -COPY T01DHEA    -PRE DHEA-                                          
010609                                                                          
010610 01  FILLER                       PIC X(16)  VALUE 'DAPP-TAB   '.         
010611*01  -COPY T01DAPP    -PRE DAPP-                                          
010612                                                                          
010613 01  FILLER                       PIC X(16)  VALUE 'VAT-TAB    '.         
010614*01  -COPY T01VAT     -PRE VAT-                                           
010630     EJECT                                                                
010631                                                                          
010632 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA'.           
010633       EXEC SQL INCLUDE T01PROC  END-EXEC.                                
010634                                                                          
010635 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA'.           
010636       EXEC SQL INCLUDE T01DHEA  END-EXEC.                                
010637                                                                          
010638 01  FILLER                       PIC X(16)  VALUE 'DAPP-AREA'.           
010639       EXEC SQL INCLUDE T01DAPP  END-EXEC.                                
010640                                                                          
010650 01  FILLER                       PIC X(16)  VALUE 'VAT-AREA '.           
010651       EXEC SQL INCLUDE T01VAT   END-EXEC.                                
010658     EJECT                                                                
010659                                                                          
010660 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
010661       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010662*                        **** STATUS-CODE FROM DB2                        
010663                                                                          
010664 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
010665 01  DB2-WS.                                                              
010666   03  SQLCODE-WS                 PIC S9(3)  VALUE ZERO.                  
010667     88  ROW-FOUND                           VALUE +000.                  
010668     88  ROW-MISSING                         VALUE +100.                  
010669   03  GOOD-SQLCODES.                                                     
010670     05  GOOD-SQLCODE OCCURS 5                                            
010671         INDEXED BY SQLCODE-IX    PIC 999.                                
010672     EJECT                                                                
010680                                                                          
010717 PROCEDURE DIVISION.                                                      
010718 MAIN SECTION.                                                            
011100     PERFORM A-INIT                                                       
011200                                                                          
011600     PERFORM B-EXECUTE                                                    
011900                                                                          
012400     PERFORM Z-FINISH                                                     
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
012910                                                                          
013000 A-INIT SECTION.                                                          
013100     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                  
013110     MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM              
014100     .                                                                    
014200     EJECT                                                                
014210                                                                          
014985 B-EXECUTE SECTION.                                                       
014986     PERFORM DB2-OPEN-CRS-MISC                                            
014994     PERFORM DB2-FETCH-CRS-MISC                                           
014996                                                                          
014997     PERFORM UNTIL ROW-MISSING                                            
014999       PERFORM BA-INIT-AMOUNTS                                            
015000       PERFORM DB2-OPEN-CRS-DAPP                                          
015001       PERFORM DB2-FETCH-CRS-DAPP                                         
015002                                                                          
015003       PERFORM UNTIL ROW-MISSING                                          
015006         PERFORM BB-ACCUMULATE-AMOUNTS                                    
015007         PERFORM DB2-FETCH-CRS-DAPP                                       
015008       END-PERFORM                                                        
015009       PERFORM DB2-CLOSE-CRS-DAPP                                         
015010                                                                          
015011       PERFORM DB2-UPDATE-DHEA                                            
015012       PERFORM DB2-FETCH-CRS-MISC                                         
015020     END-PERFORM                                                          
015103     .                                                                    
015104     EJECT                                                                
015105                                                                          
015106 BA-INIT-AMOUNTS SECTION.                                                 
015130     MOVE ZERO TO WS-SUBTO-TOT                                            
015140                  WS-SUVAT-BILLIT-TOT                                     
015202     .                                                                    
015203     EJECT                                                                
015204                                                                          
015205 BB-ACCUMULATE-AMOUNTS SECTION.                                           
015225     COMPUTE WS-SUBTO-TOT        = WS-SUBTO-TOT        +                  
015226                                   DAPP-SUBTO-APP                         
015227     END-COMPUTE                                                          
015228     COMPUTE WS-SUVAT-BILLIT-TOT = WS-SUVAT-BILLIT-TOT +                  
015229                                   DAPP-SUVAT-BILLIT-APP                  
015230     END-COMPUTE                                                          
015231     .                                                                    
015232     EJECT                                                                
015233                                                                          
015234 Z-FINISH SECTION.                                                        
015235     PERFORM DB2-CLOSE-CRS-MISC                                           
015236     .                                                                    
015237     EJECT                                                                
015238                                                                          
015239* --- DB2 SECTIONS  ---                                                   
015240*                                                                         
017210                                                                          
017250 DB2-OPEN-CRS-MISC SECTION.                                               
017251     EXEC SQL DECLARE MISC-CRS CURSOR FOR                                 
017252     SELECT   T01DHEA.IDLEGSEL,                                           
017260              T01DHEA.DAEXDAT,                                            
017261              T01DHEA.TIEXTID,                                            
017265              T01DHEA.KDVALISO,                                           
017266              T01DHEA.IDLANDX3_SEND,                                      
017267              T01DHEA.IDLEVNR,                                            
017268              T01DHEA.IDPARTNR,                                           
017269              T01DHEA.KDFINDOC,                                           
017270              T01DHEA.FLSOFT,                                             
017280              T01DHEA.FLFREE,                                             
017281              T01DHEA.FLPRIV,                                             
017290              T01DHEA.IDBREAK_1,                                          
017300              T01DHEA.IDBREAK_2                                           
017509                                                                          
017510     FROM     T01PROC,                                                    
017511              T01DHEA                                                     
017518                                                                          
017519     WHERE    T01PROC.IDSYSTEM = 'WF02'                AND                
017520              T01DHEA.IDLEGSEL = T01PROC.IDLEGSEL      AND                
017521              T01DHEA.DAEXDAT  = T01PROC.DAEXDAT       AND                
017522              T01DHEA.TIEXTID  = T01PROC.TIEXTID                          
017629     END-EXEC                                                             
017630                                                                          
017631     EXEC SQL OPEN MISC-CRS                                               
017632     END-EXEC                                                             
017633                                                                          
017634     MOVE 000            TO GOOD-SQLCODES                                 
017635     MOVE SQLCODE        TO SQLCODE-WS                                    
017636     PERFORM DB2-STATUS-CHECK                                             
017637     .                                                                    
017638     EJECT                                                                
017639                                                                          
017640 DB2-OPEN-CRS-DAPP SECTION.                                               
017641     EXEC SQL DECLARE DAPP-CRS CURSOR FOR                                 
017643     SELECT  T01DAPP.SUNTO_APP,                                           
017644             T01DAPP.SUVAT_BILLIT_APP,                                    
017645             T01DAPP.SUBTO_APP                                            
017646                                                                          
017647     FROM    T01DAPP                                                      
017648                                                                          
017649     WHERE   T01DAPP.IDLEGSEL      = :DHEA-IDLEGSEL      AND              
017650             T01DAPP.DAEXDAT       = :DHEA-DAEXDAT       AND              
017651             T01DAPP.TIEXTID       = :DHEA-TIEXTID       AND              
017652             T01DAPP.KDVALISO      = :DHEA-KDVALISO      AND              
017653             T01DAPP.IDLANDX3_SEND = :DHEA-IDLANDX3-SEND AND              
017654             T01DAPP.IDLEVNR       = :DHEA-IDLEVNR       AND              
017655             T01DAPP.IDPARTNR      = :DHEA-IDPARTNR      AND              
017656             T01DAPP.KDFINDOC      = :DHEA-KDFINDOC      AND              
017657             T01DAPP.FLSOFT        = :DHEA-FLSOFT        AND              
017658             T01DAPP.FLFREE        = :DHEA-FLFREE        AND              
017659             T01DAPP.FLPRIV        = :DHEA-FLPRIV        AND              
017660             T01DAPP.IDBREAK_1     = :DHEA-IDBREAK-1     AND              
017661             T01DAPP.IDBREAK_2     = :DHEA-IDBREAK-2     AND              
017662                                                                          
017690             EXISTS                                                       
017691                                                                          
017692            (SELECT *                                                     
017693             FROM   T01VAT                                                
017694             WHERE  T01VAT.IDLEGSEL = T01DAPP.IDLEGSEL   AND              
017695                    T01VAT.KDVAT    = T01DAPP.KDAPPEND   AND              
017696                    T01VAT.DADELDAT = '00000000')                         
017697     END-EXEC                                                             
017698                                                                          
017699     EXEC SQL OPEN DAPP-CRS                                               
017700     END-EXEC                                                             
017701                                                                          
017702     MOVE 000            TO GOOD-SQLCODES                                 
017703     MOVE SQLCODE        TO SQLCODE-WS                                    
017704     PERFORM DB2-STATUS-CHECK                                             
017705     .                                                                    
017706     EJECT                                                                
017707                                                                          
017708 DB2-FETCH-CRS-MISC SECTION.                                              
017709     EXEC SQL FETCH MISC-CRS INTO                                         
017710            :DHEA-IDLEGSEL,                                               
017711            :DHEA-DAEXDAT,                                                
017720            :DHEA-TIEXTID,                                                
017730            :DHEA-KDVALISO,                                               
017740            :DHEA-IDLANDX3-SEND,                                          
017750            :DHEA-IDLEVNR,                                                
017760            :DHEA-IDPARTNR,                                               
017770            :DHEA-KDFINDOC,                                               
017780            :DHEA-FLSOFT,                                                 
017790            :DHEA-FLFREE,                                                 
017791            :DHEA-FLPRIV,                                                 
017800            :DHEA-IDBREAK-1,                                              
017900            :DHEA-IDBREAK-2                                               
017916     END-EXEC                                                             
017917                                                                          
017920     MOVE 000100         TO GOOD-SQLCODES                                 
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
018400     EJECT                                                                
019011                                                                          
019012 DB2-FETCH-CRS-DAPP SECTION.                                              
019014     EXEC SQL FETCH DAPP-CRS INTO                                         
019019            :DAPP-SUNTO-APP,                                              
019020            :DAPP-SUVAT-BILLIT-APP,                                       
019030            :DAPP-SUBTO-APP                                               
019066     END-EXEC                                                             
019067                                                                          
019068     MOVE 000100         TO GOOD-SQLCODES                                 
019069     MOVE SQLCODE        TO SQLCODE-WS                                    
019070     PERFORM DB2-STATUS-CHECK                                             
019071     .                                                                    
019072     EJECT                                                                
019073                                                                          
019148 DB2-UPDATE-DHEA SECTION.                                                 
019150     EXEC SQL UPDATE T01DHEA                                              
019348     SET    SUVAT_BILLIT_TOT = :WS-SUVAT-BILLIT-TOT,                      
019349            SUBTO_TOT        = :WS-SUBTO-TOT                              
019350                                                                          
019351     WHERE  T01DHEA.IDLEGSEL      = :DHEA-IDLEGSEL            AND         
019352            T01DHEA.DAEXDAT       = :DHEA-DAEXDAT             AND         
019353            T01DHEA.TIEXTID       = :DHEA-TIEXTID             AND         
019354            T01DHEA.KDVALISO      = :DHEA-KDVALISO            AND         
019355            T01DHEA.IDLANDX3_SEND = :DHEA-IDLANDX3-SEND       AND         
019356            T01DHEA.IDLEVNR       = :DHEA-IDLEVNR             AND         
019357            T01DHEA.IDPARTNR      = :DHEA-IDPARTNR            AND         
019358            T01DHEA.KDFINDOC      = :DHEA-KDFINDOC            AND         
019359            T01DHEA.FLSOFT        = :DHEA-FLSOFT              AND         
019360            T01DHEA.FLFREE        = :DHEA-FLFREE              AND         
019361            T01DHEA.FLPRIV        = :DHEA-FLPRIV              AND         
019362            T01DHEA.IDBREAK_1     = :DHEA-IDBREAK-1           AND         
019363            T01DHEA.IDBREAK_2     = :DHEA-IDBREAK-2                       
019364     END-EXEC                                                             
019365                                                                          
019366     MOVE 000            TO GOOD-SQLCODES                                 
019367     MOVE SQLCODE        TO SQLCODE-WS                                    
019368     PERFORM DB2-STATUS-CHECK                                             
019369     .                                                                    
019370     EJECT                                                                
019371                                                                          
019372 DB2-CLOSE-CRS-MISC SECTION.                                              
019373     EXEC SQL CLOSE MISC-CRS                                              
019374     END-EXEC                                                             
019375     .                                                                    
019376     EJECT                                                                
019377                                                                          
019378 DB2-CLOSE-CRS-DAPP SECTION.                                              
019379     EXEC SQL CLOSE DAPP-CRS                                              
019380     END-EXEC                                                             
019381     .                                                                    
019382     EJECT                                                                
019383                                                                          
019384 DB2-STATUS-CHECK SECTION.                                                
019385     SET SQLCODE-IX         TO 1                                          
019386     SEARCH GOOD-SQLCODE AT END                                           
019387           CALL ABEND USING RKOD-ABEND-DB2                                
019390        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
019400           CONTINUE                                                       
019500     END-SEARCH                                                           
019600     .                                                                    
