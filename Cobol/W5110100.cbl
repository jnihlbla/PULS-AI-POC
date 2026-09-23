000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W5110100.                                                
000800*AUTHOR.         TOMAS SVENSSON.                                          
000900*DATE-WRITTEN.   FEB 1976.                                                
001000                                                                          
001100*        FUNKTION.                                                        
001200*                PROGRAMMET SKRIVER KONTROLLERADE                         
001300*                OCH REDIGERADE TRANSAKTIONER PÅ                          
001400*                TRANSFILEN W09225.                                       
001500*        INDATA.                                                          
001600*                TRANSAKTIONSTYPER   R94                                  
001700*        UTDATA.                                                          
001800*                TRANSFIL W09225.    DDNAMN  W09206DV.                    
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300     SELECT  W09225  ASSIGN TO UT-S-W09206DV.                             
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700     SKIP2                                                                
002800 FD  W09225                                                               
002900     RECORDING F                                                          
003000     BLOCK CONTAINS 0 RECORDS.                                            
003100     SKIP2                                                                
003200 01  W09225-UTPOST           PIC X(116).                                  
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003410                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.           
003700 77  IDPGM                   PIC X(8) VALUE 'W5110100'.                   
004100     EJECT                                                                
004200 LINKAGE SECTION.                                                         
004300     SKIP2                                                                
004400 01  TRANS-PARM.                                                          
004500     03  KOD                 PIC X(3).                                    
004600     03  FELTAB OCCURS 100.                                               
004700         05  FELKOD          PIC X(3).                                    
004800         05  FELTEXT         PIC X(15).                                   
004900     03  FILLER              PIC X(3).                                    
005000     03  INPOST              PIC X(100).                                  
005100     03  UTAREA.                                                          
005200         05  UTPOST.                                                      
005300*            07  -COPY W092W001 -PRE ID-                                  
005500             07  POST        PIC X(80).                                   
005600         05  FILLER          PIC X(134).                                  
005700     EJECT                                                                
005800 PROCEDURE DIVISION USING TRANS-PARM.                                     
006100******************************************************************        
006200*    PROGRAMMET ANROPAS DYNAMISKT AV W09206.                     *        
006300*    TRANSTYP R94 SKRIVS PÅ UTFILEN.                             *        
006400******************************************************************        
006500     SKIP2                                                                
006600 STYR SECTION.                                                            
006700     SKIP2                                                                
006800     IF LCP-ONCTR-01 =  0                                                 
006900         ADD 1 TO LCP-ONCTR-01                                            
007000                               OPEN OUTPUT W09225                         
007010     END-IF                                                               
007100     IF KOD = 'EOF'                                                       
007200       CLOSE W09225                                                       
007300       GOBACK                                                             
007400     END-IF                                                               
007500     SKIP2                                                                
007600     IF ID-IDPTYP = 'R94'                                                 
007700       WRITE W09225-UTPOST FROM UTPOST                                    
007800     END-IF                                                               
007900     GOBACK                                                               
008000     .                                                                    
