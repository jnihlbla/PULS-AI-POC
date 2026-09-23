003095 ID DIVISION.                                                             
003096     SKIP2                                                                
003097 PROGRAM-ID.     W9600620.                                                
003098 AUTHOR.         KARIN OLSSON.                                            
003099 DATE-WRITTEN.   92/12/18.                                                
003100                                                                          
003101     REMARKS.                                                             
003102*                                                                         
003103*    FUNKTION:                                                            
003104*        PROGRAM FÖR ATT RETURNERA EN RAD I EN KOMPILERINGSLISTA.         
003105*                                                                         
003106*    RETURKODER:                                                          
003107*        4 OM TILLFÄLLIGT SLUT PÅ DATA                                    
003108*        8 OM SLUT PÅ DATA                                                
003109*       16 OM I/O-ERROR                                                   
003110*                                                                         
003111     SKIP3                                                                
003112 ENVIRONMENT DIVISION.                                                    
003113     SKIP2                                                                
003114 INPUT-OUTPUT SECTION.                                                    
003115     EJECT                                                                
003116 DATA DIVISION.                                                           
003117     SKIP3                                                                
003118 WORKING-STORAGE SECTION.                                                 
003119     SKIP2                                                                
003120                                                                          
003121*    -- CHECKED BY WY2000                                                 
003122 77  IDPGM                       PIC X(8)    VALUE 'W9600620'.            
003130 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003201 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
003203     SKIP2                                                                
003204 01  GENERELLA-SUBPROGRAM.                                                
003205   03  ISPLINK                   PIC X(8)    VALUE 'ISPLINK '.            
003206   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
003208     SKIP2                                                                
003209 77  WDUMMY                      PIC X(8)    VALUE SPACE.                 
003211 77  ISP-VDEFINE                 PIC X(8)    VALUE 'VDEFINE '.            
003212 77  ISP-VDELETE                 PIC X(8)    VALUE 'VDELETE '.            
003214 77  ISP-VPUT                    PIC X(8)    VALUE 'VPUT    '.            
003215 77  ISP-SHARED                  PIC X(8)    VALUE 'SHARED  '.            
003221 77  PACK                        PIC X(8)    VALUE 'PACK    '.            
003222 77  CHAR                        PIC X(8)    VALUE 'CHAR    '.            
003223 77  FIXED                       PIC X(8)    VALUE 'FIXED   '.            
003224 77  VDEFINE-OPT                 PIC X(16)                                
003225                              VALUE '(COPY NOBSCAN)'.                     
003230     SKIP2                                                                
011816 01  RADLRECL                    PIC 9(3)    COMP-3.                      
011817 01  N-RADLRECL                  PIC X(8)    VALUE 'RADLRECL'.            
011818 01  L-RADLRECL                  PIC S9(9)   COMP VALUE +2.               
011819     SKIP2                                                                
011820 01  TOPRADNR                    PIC 9(5)    COMP-3.                      
011821 01  N-TOPRADNR                  PIC X(8)    VALUE 'TOPRADNR'.            
011822 01  L-TOPRADNR                  PIC S9(9)   COMP VALUE +3.               
011823     SKIP2                                                                
011824 01  MAXRADNR                    PIC 9(5)    COMP-3.                      
011825 01  N-MAXRADNR                  PIC X(8)    VALUE 'MAXRADNR'.            
011826 01  L-MAXRADNR                  PIC S9(9)   COMP VALUE +3.               
011827     SKIP2                                                                
011835 01  TAB-RADNR                   PIC 9(8)    COMP.                        
011836     SKIP2                                                                
011840 01  RKOD                        PIC S9(4)   COMP.                        
011850     EJECT                                                                
011900 LINKAGE SECTION.                                                         
012020     SKIP2                                                                
012100 01  LIST-RAD-ADRESS             PIC S9(8)   COMP SYNC.                   
012200 01  LIST-LAENGD                 PIC 9(8)    COMP.                        
012300 01  RADNR                       PIC 9(8)    COMP.                        
012310 01  DATA-ADRESS                 PIC S9(8)   COMP SYNC.                   
016200                                                                          
016700     EJECT                                                                
016800 PROCEDURE DIVISION USING LIST-RAD-ADRESS LIST-LAENGD                     
016810                        RADNR DATA-ADRESS.                                
016900     SKIP2                                                                
017000     MOVE ZERO TO RKOD                                                    
017100                                                                          
017122     CALL ISPLINK USING ISP-VDELETE N-RADLRECL                            
017123     CALL ISPLINK USING ISP-VDELETE N-TOPRADNR                            
017124     CALL ISPLINK USING ISP-VDELETE N-MAXRADNR                            
017125                                                                          
017133     CALL ISPLINK USING ISP-VDEFINE N-RADLRECL RADLRECL PACK              
017134                         L-RADLRECL VDEFINE-OPT                           
017135     IF RETURN-CODE > 0                                                   
017136       DISPLAY 'SAKNAR RADLRECL'                                          
017137       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017138     END-IF                                                               
017139                                                                          
017140     CALL ISPLINK USING ISP-VDEFINE N-TOPRADNR TOPRADNR PACK              
017141                         L-TOPRADNR VDEFINE-OPT                           
017142     IF RETURN-CODE > 0                                                   
017143       DISPLAY 'SAKNAR TOPRADNR'                                          
017144       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017145     END-IF                                                               
017146                                                                          
017147     CALL ISPLINK USING ISP-VDEFINE N-MAXRADNR MAXRADNR PACK              
017148                         L-MAXRADNR VDEFINE-OPT                           
017149     IF RETURN-CODE > 0                                                   
017150       DISPLAY 'SAKNAR MAX ANTAL RADER'                                   
017151       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017152     END-IF                                                               
017153                                                                          
017170     MOVE RADLRECL TO LIST-LAENGD                                         
017171                                                                          
017176     IF RADNR = 99999999                                                  
017177       MOVE MAXRADNR TO RADNR                                             
017178       MOVE 4 TO RKOD                                                     
017179     ELSE                                                                 
017218       IF RADNR > MAXRADNR                                                
017219         MOVE MAXRADNR TO RADNR                                           
017220         COMPUTE TAB-RADNR = (TOPRADNR + RADNR) - 1                       
017221         COMPUTE LIST-RAD-ADRESS =                                        
017222                       DATA-ADRESS + 150 * TAB-RADNR                      
017223         MOVE 8 TO RKOD                                                   
017230       ELSE                                                               
017411         COMPUTE TAB-RADNR = (TOPRADNR + RADNR) - 1                       
017428         COMPUTE LIST-RAD-ADRESS =                                        
017429                       DATA-ADRESS + 150 * TAB-RADNR                      
017434       END-IF                                                             
017435     END-IF                                                               
017438                                                                          
017480     MOVE RKOD TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
