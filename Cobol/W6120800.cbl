000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6120800.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   01/08/10.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR FIL FÖR BINNING PRODUCTIVITY REPORT                       
001100*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002402*          --- NEDLÄST R32                                                
002403     SELECT INFIL                      ASSIGN TO W61208D1.                
002404     SKIP2                                                                
002405*          --- SUMMERING FÖR UPPFÖLJNINGSLISTA                            
002410     SELECT UTFIL                      ASSIGN TO W61208D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  INFIL                                                                
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  -COPY W61207      -L.                                                
003007     SKIP3                                                                
003008 FD  UTFIL                                                                
003009     RECORDING       F                                                    
003010     BLOCK CONTAINS  0.                                                   
003011                                                                          
003020*01  POST -COPY W61208 -PRE  UT-  -L.                                     
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W6120800'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
003800 77  SPAR-IDUSER-003             PIC X(5)    VALUE SPACE.                 
003809 77  WS-KVANT                    PIC S9(7)   VALUE ZERO COMP-3.           
003810                                                                          
003830 01  WS-ADLAGOMR                 PIC 9(2).                                
003831 01  FILLER REDEFINES WS-ADLAGOMR.                                        
003840     03 WS-ADLAGOMR-POS1         PIC 9(1).                                
003850     03 FILLER                   PIC 9(1).                                
003851                                                                          
003860 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
003870     88  END-OF-INFIL                        VALUE 'J'.                   
003880                                                                          
004000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004500                                                                          
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
005010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005700                                                                          
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006100     EJECT                                                                
006200*    --- PARAMETRAR TILL DATKORT                                          
006300*                                                                         
006400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61208'.              
006500     SKIP2                                                                
006600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
006700     SKIP2                                                                
006800*01  -COPY WDATKORT                                                       
006901     EJECT                                                                
006902*    --- PARAMETRAR TILL POSTSUM                                          
006903*                                                                         
006910*01  -COPY W0005   -PRE  POSTSUM-                                         
007101     EJECT                                                                
007102 01  IN-AREA-START               PIC X(24)   VALUE                        
007103                                 'IN-AREA-START  '.                       
007104     SKIP2                                                                
007105                                                                          
007106*01  AREA -COPY W61207     -PRE IN-                                       
007107     EJECT                                                                
007108 01  UT-AREA-START               PIC X(24)   VALUE                        
007109                                 'UT-AREA-START  '.                       
007110     SKIP2                                                                
007111                                                                          
007120*01  AREA -COPY W61208     -PRE UT-                                       
007200     EJECT                                                                
007300 PROCEDURE DIVISION.                                                      
007400 MAIN SECTION.                                                            
007700                                                                          
007800     PERFORM A-INIT                                                       
007810                                                                          
007910     PERFORM S01-LAES-INFIL                                               
007911     MOVE IN-IDDC TO SPAR-IDDC                                            
007912                     UT-IDDC                                              
007913     MOVE IN-IDUSER-003 TO SPAR-IDUSER-003                                
007914                           UT-IDUSER-003                                  
007920                                                                          
008000     PERFORM UNTIL END-OF-INFIL                                           
008100        IF IN-IDDC = SPAR-IDDC                                            
008110        AND IN-IDUSER-003 = SPAR-IDUSER-003                               
008300           PERFORM B-BEHANDLA                                             
008701        ELSE                                                              
008702           PERFORM S02-SKRIV-UTPOST                                       
008703           PERFORM S03-NOLLSTALL                                          
008704           MOVE IN-IDDC TO SPAR-IDDC                                      
008705                           UT-IDDC                                        
008706           MOVE IN-IDUSER-003 TO SPAR-IDUSER-003                          
008707                           UT-IDUSER-003                                  
008708           PERFORM B-BEHANDLA                                             
008709        END-IF                                                            
008710        PERFORM S01-LAES-INFIL                                            
008800     END-PERFORM                                                          
008900                                                                          
008910     IF UT-IDUSER-003 NOT = SPACE                                         
008920        PERFORM S02-SKRIV-UTPOST                                          
008930     END-IF                                                               
009000                                                                          
009100     PERFORM Z-FINIT                                                      
009200                                                                          
009300     MOVE ZERO TO RETURN-CODE                                             
009400     GOBACK                                                               
009500     .                                                                    
009600     EJECT                                                                
009700 A-INIT SECTION.                                                          
009801                                                                          
009810     OPEN INPUT  INFIL                                                    
009910     OPEN OUTPUT UTFIL                                                    
010000                                                                          
010100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
010200     MOVE D-AAR    TO  DAGENS-DATUM-AAR                                   
010300     MOVE D-MAANAD TO  DAGENS-DATUM-MAANAD                                
010400     MOVE D-DAG    TO  DAGENS-DATUM-DAG                                   
010510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010520     MOVE DAGENS-DATUM TO UT-TIAAMMDD                                     
010530     PERFORM S03-NOLLSTALL                                                
010600     .                                                                    
010700     EJECT                                                                
010710 B-BEHANDLA SECTION.                                                      
010728                                                                          
010729     MOVE IN-ADLAGOMR TO WS-ADLAGOMR                                      
010730     EVALUATE TRUE                                                        
010731        WHEN WS-ADLAGOMR-POS1 = 1                                         
010732             ADD +1 TO UT-KVANTAL-1                                       
010733        WHEN WS-ADLAGOMR-POS1 = 2                                         
010734             ADD +1 TO UT-KVANTAL-2                                       
010735        WHEN WS-ADLAGOMR-POS1 = 3                                         
010736             ADD +1 TO UT-KVANTAL-3                                       
010737        WHEN WS-ADLAGOMR-POS1 = 4                                         
010738             ADD +1 TO UT-KVANTAL-4                                       
010739        WHEN WS-ADLAGOMR-POS1 = 9                                         
010740             ADD +1 TO UT-KVANTAL-9                                       
010741        WHEN OTHER                                                        
010742             ADD +1 TO UT-KVANTAL-OVR                                     
010743     END-EVALUATE                                                         
010744                                                                          
010748     IF IN-KVANTMOT > 0                                                   
010751        ADD +1 TO UT-KVRADER-BIN                                          
010752     END-IF                                                               
010753                                                                          
010754     IF IN-KVART-SKROT > 0                                                
010757        ADD +1 TO UT-KVRADER-DAM                                          
010758     END-IF                                                               
010760                                                                          
010761     MOVE IN-KVANTMOT TO WS-KVANT                                         
010762     ADD IN-KVART-SKROT TO WS-KVANT                                       
010764     IF IN-KVAVIS > WS-KVANT                                              
010768        ADD +1 TO UT-KVRADER-SHORT                                        
010772     ELSE                                                                 
010773        IF IN-KVAVIS < WS-KVANT                                           
010774           ADD +1 TO UT-KVRADER-OVER                                      
010777        END-IF                                                            
010778     END-IF                                                               
010779     .                                                                    
010780     EJECT                                                                
010800 Z-FINIT SECTION.                                                         
010900                                                                          
010901     CLOSE INFIL                                                          
010910           UTFIL                                                          
011001                                                                          
011002     MOVE 'S' TO POSTSUM-OPKOD                                            
011010     CALL POSTSUM USING POSTSUM-PARM                                      
011100     .                                                                    
011201     SKIP3                                                                
011202 S01-LAES-INFIL   SECTION.                                                
011203                                                                          
011204     READ INFIL INTO IN-AREA                                              
011205     AT END                                                               
011207        SET END-OF-INFIL TO TRUE                                          
011208                                                                          
011209     NOT AT END                                                           
011210        MOVE 'INFIL'   TO POSTSUM-FDNAMN                                  
011211        MOVE 'W61208D1' TO POSTSUM-DDNAMN2                                
011214        MOVE SPACE      TO POSTSUM-TRANSTYP                               
011215        CALL POSTSUM USING POSTSUM-PARM                                   
011216     END-READ                                                             
011220     .                                                                    
011301     EJECT                                                                
011302 S02-SKRIV-UTPOST SECTION.                                                
011303                                                                          
011304     WRITE UT-POST FROM UT-AREA                                           
011305                                                                          
011306     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
011307     MOVE 'UTFIL'    TO POSTSUM-FDNAMN                                    
011308     MOVE 'W61208D2' TO POSTSUM-DDNAMN2                                   
011309     CALL POSTSUM USING POSTSUM-PARM                                      
011310     .                                                                    
011500     SKIP3                                                                
011510 S03-NOLLSTALL SECTION.                                                   
011520                                                                          
011530     MOVE SPACE TO UT-IDDC                                                
011540                   UT-IDUSER-003                                          
011550     MOVE ZERO  TO UT-KVRADER-BIN                                         
011560                   UT-KVRADER-SHORT                                       
011570                   UT-KVRADER-OVER                                        
011580                   UT-KVRADER-DAM                                         
011590                   UT-KVANTAL-1                                           
011591                   UT-KVANTAL-2                                           
011592                   UT-KVANTAL-3                                           
011593                   UT-KVANTAL-4                                           
011594                   UT-KVANTAL-9                                           
011595                   UT-KVANTAL-OVR                                         
011602                   WS-KVANT                                               
011603     .                                                                    
