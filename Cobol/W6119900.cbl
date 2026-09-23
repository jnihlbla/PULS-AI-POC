000300 ID DIVISION.                                                             
000400 PROGRAM-ID. W6119900.                                                    
000800*AUTHOR.         TOMAS 4103.                                              
000900*DATE-WRITTEN.   MARS 1976.                                               
001000*REMARKS.                                                                 
001100*        FUNKTION.                                                        
001200*                PROGRAMMET SKRIVER KONTROLLERADE                         
001300*                OCH REDIGERADE TRANSAKTIONER PÅ                          
001400*                TRANSFILERNA.                                            
001500*                R32:OR GENOMGÅR KOPPLADE KONTROLLER.                     
001600*        INDATA.                                                          
001700*                TRANSAKTIONSTYPER                                        
002200*                                    R40                                  
002400*        UTDATA.                                                          
002500*                TRANSFIL W09210.    DDNAMN  W09206DC.                    
002800     EJECT                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000 INPUT-OUTPUT SECTION.                                                    
003100 FILE-CONTROL.                                                            
003200     SKIP3                                                                
003300     SELECT W09210  ASSIGN W09206DC.                                      
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 FILE SECTION.                                                            
003700 FD  W09210                                                               
003800     BLOCK CONTAINS 0 RECORDS                                             
003900     RECORDING V.                                                         
004000                                                                          
004100     SKIP2                                                                
004200 01  FILLER  PIC X(79).                                                   
005000     SKIP2                                                                
005100*01  W211R40 -COPY W211R40 -PRE UT- -L.                                   
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005410                                                                          
005500*    -- CHECKED BY WY2000                                                 
005600 77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.           
006100*                                                                         
008000     EJECT                                                                
008100 LINKAGE SECTION.                                                         
008200 01  TRANS-PARM.                                                          
008300     03  EOF-KOD             PIC X(3).                                    
008400     03  FELTAB  OCCURS 100.                                              
008500         05  FELKOD          PIC X(3).                                    
008600         05  FELTEXT         PIC X(15).                                   
008700     03  FILLER              PIC X(3).                                    
008800     03  INPOST              PIC X(100).                                  
009100     03  UTAREA.                                                          
009200*        05  DEL     -COPY W092W001  -PRE ID-.                            
009400         05  UTPOST          PIC X(100).                                  
010100*        05  -COPY W211R40 -PRE R40-    -RED UTPOST.                      
010300     EJECT                                                                
010400 PROCEDURE DIVISION USING TRANS-PARM.                                     
010600                                                                          
010700******************************************************************        
010800*    PROGRAMMET ANROPAS DYNAMISKT AV W09206.                     *        
011100******************************************************************        
011200                                                                          
011300 STYR SECTION.                                                            
011400                                                                          
011600     IF LCP-ONCTR-01 =  0                                                 
011700       ADD 1 TO LCP-ONCTR-01                                              
011800       OPEN OUTPUT W09210                                                 
011810     END-IF                                                               
011900     IF EOF-KOD = 'EOF'                                                   
012000       CLOSE W09210                                                       
012100       GOBACK                                                             
012200     END-IF                                                               
012300                                                                          
012400     EVALUATE ID-IDPTYP                                                   
013500     WHEN 'R40'                                                           
013610       WRITE UT-W211R40 FROM R40-W211R40                                  
013700     END-EVALUATE                                                         
013800                                                                          
013900     GOBACK.                                                              
