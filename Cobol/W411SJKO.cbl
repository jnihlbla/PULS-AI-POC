000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.             W411SJKO.                                        
000500 AUTHOR.                 ELEONOR ÖSTRÖM                                   
000600     DATE-WRITTEN.       FEB 2002.                                        
000700*                                                                         
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001010*        ANDVÄNDS FÖR DDI-KUNDER DÄR PRIS EJ FINNS I RAD.                 
001100*        HÄMTAR ARTIKELNS SJÄLVKOST FRÅN WDK6 OCH RÄKNAR                  
001200*        UT PRELIMINÄRT PRIS TILL DDI-KUNDER.                             
001300*        (PRELIMINÄRT PRIS = PRARTSJK X 4)                                
002400*                                                                         
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900                                                                          
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200 77  PROGRAM-NAMN            PIC X(8) VALUE 'W411SJKO'.                   
003300     SKIP2                                                                
003400*    ---- KONSTANTER                                                      
003500                                                                          
003600 77  JA                      PIC X       VALUE 'J'.                       
003700 77  NEJ                     PIC X       VALUE 'N'.                       
003800 77  RKOD-16                 PIC S9(4)   VALUE +16  COMP.                 
003900 77  WS-LOCPREL              PIC S9(7)V9(2) VALUE +0  COMP-3.             
004600*                                                                         
006000     EJECT                                                                
006700                                                                          
006800 77  AVSLUTA-SW              PIC  X(01)  VALUE 'N'.                       
006900     88 AVSLUTA                          VALUE 'J'.                       
007000                                                                          
007300     SKIP2                                                                
007400*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
007500     SKIP2                                                                
007600 01  DYNAMISKA-SUBPROGRAM.                                                
007800   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
007900   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
008000   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
008100   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
008200     SKIP2                                                                
008700     EJECT                                                                
008800*01  -COPY WDATAREA.                                                      
009000     EJECT                                                                
009100 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
010800                                                                          
010900 01  NYCKLAR-TILL-DLI.                                                    
011000                                                                          
011400   03  W-WDK6-IDARTNR-X.                                                  
011500     05  W-WDK6-IDARTNR      PIC S9(9)    COMP-3.                         
011600                                                                          
011700                                                                          
011800 01  STATUS-WS               PIC XX.                                      
011900     88  SEGMENT-FINNS                    VALUE '  '.                     
011910     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
011930     SKIP2                                                                
011940 01  GODK-STATUSKODER.                                                    
011950   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
011960     SKIP2                                                                
011970 01  SSA1                    PIC X(64).                                   
012000     EJECT                                                                
012020*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
013310 01  FILLER                  PIC X(16) VALUE 'WDK601-IO-AREA'.            
013320 01  WDK601-IO-AREA.                                                      
013330*03  -COPY WDK601                                                         
013340     EJECT                                                                
013360 01  FILLER                  PIC X(16) VALUE 'WDK611-IO-AREA'.            
013370 01  WDK611-IO-AREA.                                                      
013380*03  -COPY WDK611                                                         
013400     EJECT                                                                
013700 LINKAGE SECTION.                                                         
013800     SKIP2                                                                
013900*01  -COPY W411SJKO                                                       
014000     EJECT                                                                
014300*01  -COPY W0008      -PRE  WDK6-                                         
014400       05  FILLER                PIC X.                                   
014800     EJECT                                                                
014900 PROCEDURE DIVISION  USING  SJKO-W411SJKO                                 
015000                            WDK6-PCB.                                     
015100                                                                          
015200 STYR SECTION.                                                            
015300                                                                          
015400     PERFORM A-INIT                                                       
015500                                                                          
016100     PERFORM B-HAMTA-PRARTSJK                                             
016200                                                                          
016201     IF SJKO-KDCALL = 1                                                   
016210       PERFORM C-RAKNA-UT-LOCPREL                                         
016211     ELSE                                                                 
016212       PERFORM D-HAMTA-KVROS                                              
016213     END-IF                                                               
016214                                                                          
016220                                                                          
016400                                                                          
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT       SECTION.                                                    
017700     SKIP2                                                                
017710     MOVE SJKO-IDARTNR         TO W-WDK6-IDARTNR                          
017720                                                                          
017800     MOVE +0                   TO SJKO-PRARTSJK                           
017900     MOVE +0                   TO WS-LOCPREL                              
018600     .                                                                    
018700     EJECT                                                                
018800 B-HAMTA-PRARTSJK SECTION.                                                
018810                                                                          
018900     SKIP2                                                                
018910                                                                          
019000     PERFORM IMS-GU-WDK601                                                
019100     PERFORM IMS-GNP-WDK611                                               
019400     .                                                                    
019500     EJECT                                                                
021200 C-RAKNA-UT-LOCPREL   SECTION.                                            
021300                                                                          
021320     COMPUTE WS-LOCPREL ROUNDED   = CLAG-PRARTSJK * 4                     
021322                                                                          
021330     MOVE    WS-LOCPREL    TO SJKO-PRARTSJK                               
021340     MOVE    ZERO          TO SJKO-KVROS                                  
023600     .                                                                    
023700     EJECT                                                                
023800 D-HAMTA-KVROS        SECTION.                                            
023900                                                                          
024600     MOVE    ZERO          TO SJKO-PRARTSJK                               
024700     MOVE    CLAG-KVROS    TO SJKO-KVROS                                  
024800     .                                                                    
024900     EJECT                                                                
057410 IMS-GU-WDK601 SECTION.                                                   
057420                                                                          
057430     STRING 'WDK601  (IDARTNR  =' W-WDK6-IDARTNR-X ')'                    
057440          DELIMITED BY SIZE INTO SSA1                                     
057450     MOVE '  '            TO GODK-STATUSKODER                             
057460     CALL CBLTDLI USING GU WDK6-PCB WDK601-IO-AREA SSA1                   
057470     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
057480     PERFORM IMS-STATUSKONTROLL                                           
057490     .                                                                    
057491     SKIP3                                                                
057492 IMS-GNP-WDK611 SECTION.                                                  
057493                                                                          
057494     MOVE   'WDK611  ' TO SSA1                                            
057495     MOVE '  '         TO GODK-STATUSKODER                                
057496     CALL CBLTDLI USING GNP WDK6-PCB WDK611-IO-AREA SSA1                  
057497     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
057498     PERFORM IMS-STATUSKONTROLL                                           
057499     .                                                                    
057500     EJECT                                                                
060500 IMS-STATUSKONTROLL SECTION.                                              
060600                                                                          
060700     SET STATUS-IX TO 1                                                   
060800     SEARCH GODK-STATUS                                                   
060900       AT END CALL FELLOG                                                 
061000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
061100     END-SEARCH                                                           
061200     .                                                                    
061310     EJECT                                                                
