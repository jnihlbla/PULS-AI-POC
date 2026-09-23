000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6142200.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   AUG 2003.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER FIL W614PP MED URVAL FRÅN BILD 6308                        
000900*        SKAPAR UTFIL TILL MAIL.                                          
001000*                                                                         
001300                                                                          
001400     SKIP3                                                                
001410 ENVIRONMENT DIVISION.                                                    
001420     SKIP2                                                                
001430 INPUT-OUTPUT SECTION.                                                    
001440                                                                          
001450 FILE-CONTROL.                                                            
001460     SKIP2                                                                
001470*          --- URVAL FRÅN 6308                                            
001480     SELECT W614PP                     ASSIGN TO W61422D1.                
001490     SKIP2                                                                
001500*          --- UTFIL FÖR MAIL                                             
001600     SELECT UTFIL                      ASSIGN TO W61422D2.                
001700     EJECT                                                                
001800 DATA DIVISION.                                                           
001900     SKIP2                                                                
002000 FILE SECTION.                                                            
002100     SKIP3                                                                
002200 FD  W614PP                                                               
002300     RECORDING       F                                                    
002400     BLOCK CONTAINS  0.                                                   
002500                                                                          
002600 01  PARM            PIC X(80).                                           
002700     EJECT                                                                
002800 FD  UTFIL                                                                
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200 01  UT-POST         PIC X(80).                                           
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W6142200'.            
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
004000 77  MAX-IX                      PIC S9(3)   VALUE +5   COMP-3.           
004100 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004200                                                                          
004800 01  DYNAMISKA-SUBPROGRAM.                                                
004910     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004920     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004930                                                                          
004940 01  FELTEXT.                                                             
004950     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004960     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004970     EJECT                                                                
004980*    --- PARAMETRAR TILL POSTSUM                                          
004990*01  -COPY W0005   -PRE  POSTSUM-                                         
005000     EJECT                                                                
005010 01  PARM-AREA-START             PIC X(24)   VALUE                        
005020                                 'PARM-AREA-START '.                      
005040 01  PARM-AREA                   PIC X(300).                              
005041 01  FILLER REDEFINES PARM-AREA.                                          
005050     03 PARM-TABELL.                                                      
005060        05 PARM-TAB-RAD OCCURS 5.                                         
005062           07 PARM-IDMAIL        PIC X(60).                               
005066                                                                          
005067 01  MAIL-RAD.                                                            
005069     03 FILLER                   PIC X(5) VALUE 'DEST '.                  
005070     03 MAIL-ADRESS              PIC X(60).                               
005094     EJECT                                                                
005900 01  UT-AREA-START               PIC X(24)   VALUE                        
006000                                 'UT-AREA-START '.                        
006100 01  UT-AREAN.                                                            
006200     03  UT-AREA                 PIC X(80).                               
006300     EJECT                                                                
009800 PROCEDURE DIVISION.                                                      
010100                                                                          
010200     PERFORM A-INIT                                                       
010300                                                                          
010400     PERFORM S11-LAES-W614PP                                              
010700     PERFORM B-SKAPA-UTFIL                                                
010900                                                                          
010910     PERFORM Z-FINIT                                                      
010921     MOVE ZERO TO RETURN-CODE                                             
010922     GOBACK                                                               
010923     .                                                                    
010924     EJECT                                                                
010925 A-INIT SECTION.                                                          
010926                                                                          
010927     OPEN INPUT W614PP                                                    
010928     OPEN OUTPUT UTFIL                                                    
010929     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010930     MOVE SPACE TO UT-AREA                                                
010940     MOVE SPACE TO MAIL-ADRESS                                            
010960     .                                                                    
010970     EJECT                                                                
010980 B-SKAPA-UTFIL SECTION.                                                   
010990                                                                          
011000     MOVE ')SEND' TO UT-AREA                                              
011010     PERFORM S12-SKRIV-UTPOST                                             
011020     MOVE 'TITLE LEVERANSSPÄRR' TO UT-AREA                                
011030     PERFORM S12-SKRIV-UTPOST                                             
011040     MOVE 'OPTION FORCE' TO UT-AREA                                       
011050     PERFORM S12-SKRIV-UTPOST                                             
011060                                                                          
011070     IF PARM-TAB-RAD(2) = SPACE                                           
011071        CONTINUE                                                          
011072     ELSE                                                                 
011073        MOVE PARM-IDMAIL(2) TO MAIL-ADRESS                                
011074        MOVE MAIL-RAD TO UT-AREA                                          
011075        PERFORM S12-SKRIV-UTPOST                                          
011078     END-IF                                                               
011079     IF PARM-TAB-RAD(3) = SPACE                                           
011080        CONTINUE                                                          
011081     ELSE                                                                 
011082        MOVE PARM-IDMAIL(3) TO MAIL-ADRESS                                
011083        MOVE MAIL-RAD TO UT-AREA                                          
011084        PERFORM S12-SKRIV-UTPOST                                          
011090     END-IF                                                               
011100     IF PARM-TAB-RAD(4) = SPACE                                           
011200        CONTINUE                                                          
011300     ELSE                                                                 
011400        MOVE PARM-IDMAIL(4) TO MAIL-ADRESS                                
011410        MOVE MAIL-RAD TO UT-AREA                                          
011500        PERFORM S12-SKRIV-UTPOST                                          
011600     END-IF                                                               
011700     IF PARM-TAB-RAD(5) = SPACE                                           
011800        CONTINUE                                                          
011900     ELSE                                                                 
012000        MOVE PARM-IDMAIL(5) TO MAIL-ADRESS                                
012010        MOVE MAIL-RAD TO UT-AREA                                          
012100        PERFORM S12-SKRIV-UTPOST                                          
012200     END-IF                                                               
012700     MOVE 'MEMO SEND' TO UT-AREA                                          
012800     PERFORM S12-SKRIV-UTPOST                                             
012900     MOVE ')END' TO UT-AREA                                               
013000     PERFORM S12-SKRIV-UTPOST                                             
014300     .                                                                    
014310     EJECT                                                                
014320 Z-FINIT SECTION.                                                         
014330                                                                          
014340     CLOSE W614PP                                                         
014350           UTFIL                                                          
014360                                                                          
014370     MOVE 'S' TO POSTSUM-OPKOD                                            
014380     CALL POSTSUM USING POSTSUM-PARM                                      
014390     .                                                                    
014400     EJECT                                                                
014470 S11-LAES-W614PP SECTION.                                                 
014480                                                                          
014490     READ W614PP INTO PARM-TAB-RAD(1)                                     
014491     READ W614PP INTO PARM-TAB-RAD(2)                                     
014492     READ W614PP INTO PARM-TAB-RAD(3)                                     
014493     READ W614PP INTO PARM-TAB-RAD(4)                                     
014494     READ W614PP INTO PARM-TAB-RAD(5)                                     
014500     .                                                                    
014510     EJECT                                                                
014520 S12-SKRIV-UTPOST SECTION.                                                
014530                                                                          
014540     WRITE UT-POST FROM UT-AREA                                           
014541     MOVE SPACE TO MAIL-ADRESS                                            
014542     MOVE SPACE TO UT-AREA                                                
014550                                                                          
014551     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014552     MOVE 'UTFIL  '  TO POSTSUM-FDNAMN                                    
014553     MOVE 'W61422D2' TO POSTSUM-DDNAMN2                                   
014554     CALL POSTSUM USING POSTSUM-PARM                                      
014555     .                                                                    
014556     EJECT                                                                
