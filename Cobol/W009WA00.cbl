000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W009WA00.                                                 
000400 AUTHOR.        KJELL ANDRE                                               
000500     DATE-WRITTEN.  MARS 1988.                                            
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAM FÖR ATT FÅ ETT JOBB ATT LIGGA I WAIT EN VISS             
001000*        TID.                                                             
001100*        PROGRAMMET KÖRS NORMALT VIA PROCEDUR WWAIT.                      
001200*                                                                         
001300*        FRÅN EXEC-PARM HÄMTAS UPPGIFT OM HUR MÅNGA SEKUNDER              
001400*        PROGRAMMET SKA VÄNTA.                                            
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 - OM EXEC-PARM ÄR FELAKTIG.                                
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200     SKIP2                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400     SKIP2                                                                
002401                                                                          
002410*    -- CHECKED BY WY2000                                                 
002500 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W009WA00'.            
002600     SKIP3                                                                
002700*    --- ARBETSFÄLT                                                       
002800 77  SEKUNDER-NUM                PIC 9(5).                                
002900 77  SEKUNDER-X                  PIC X(5)    JUSTIFIED RIGHT.             
003000     SKIP3                                                                
003100 01  DYNAMISKA-SUBPROGRAM.                                                
003200*                                                                         
003300     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
003400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
003500     SKIP2                                                                
003600*    --- PARAMETRAR TILL W009WAIT                                         
003700                                                                          
003800 77  HUNDRADELS                  PIC S9(9)   COMP VALUE ZERO.             
003900     SKIP2                                                                
004000*    --- PARAMETRAR TILL ABEND                                            
004100                                                                          
004200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
004300     EJECT                                                                
004400 LINKAGE SECTION.                                                         
004500     SKIP2                                                                
004600 01  EXEC-PARM.                                                           
004700     03  EXEC-LAENGD             PIC S9(4) COMP.                          
004800     03  EXEC-VAERDE             PIC X(5).                                
004900     EJECT                                                                
005000 PROCEDURE DIVISION USING EXEC-PARM.                                      
005100     SKIP2                                                                
005200     UNSTRING EXEC-VAERDE DELIMITED BY '/'                                
005300     INTO SEKUNDER-X                                                      
005400     INSPECT SEKUNDER-X REPLACING LEADING SPACE BY ZERO                   
005500                                                                          
005600     IF SEKUNDER-X NOT NUMERIC                                            
005700       DISPLAY 'INVALID WAIT TIME: ' EXEC-VAERDE                          
005800       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
005900     ELSE                                                                 
006000       MOVE SEKUNDER-X TO SEKUNDER-NUM                                    
006100       COMPUTE HUNDRADELS = SEKUNDER-NUM * 100                            
006200       DISPLAY 'WAITING ' SEKUNDER-NUM ' SECONDS'                         
006300       CALL W009WAIT USING HUNDRADELS                                     
006400     END-IF                                                               
006500                                                                          
006600     MOVE ZERO TO RETURN-CODE                                             
006700     GOBACK                                                               
006800     .                                                                    
