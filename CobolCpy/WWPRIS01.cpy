000100*** EDIT ALLOWED                                                          
000200*                            *************************************        
000300*                            *** ANVÄNDS VID TEST AV:                     
000400*                            ***  - VILKEN MARKNADSBOLAGVALUTA OCH        
000500*                            ***    KOSTNADSVALUTA ETT VISST              
000600*                            ***    MARKNADSBOLAG HAR                     
000600*                            ***                                          
000800 01  WWPRIS01-GRUPP.                                                      
000900*                                                                         
001000*                                                                         
001100     05  WWPRIS01-IDMARKBO-GRUPP.                                         
001400*                                                                         
001200*        NORDEN M1                                                        
001200         10  FILLER            PIC X(1)     VALUE 'A'.                    
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001400*                                                                         
001200*        VCEM M2                                                          
001200         10  FILLER            PIC X(1)     VALUE 'B'.                    
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001400*                                                                         
001200*        VCI M3                                                           
001200         10  FILLER            PIC X(1)     VALUE 'C'.                    
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001400*                                                                         
001200*        VCSA M4                                                          
001200         10  FILLER            PIC X(1)     VALUE 'D'.                    
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001400*                                                                         
001200*        VCNA M5                                                          
001200         10  FILLER            PIC X(1)     VALUE 'E'.                    
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001400*                                                                         
001200*        ASIEN M0                                                         
001200         10  FILLER            PIC X(1)     VALUE 'F'.                    
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001400*                                                                         
001200*        VCI-EAST M6                                                      
001200         10  FILLER            PIC X(1)     VALUE 'G'.                    
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001300         10  FILLER            PIC X(3)     VALUE 'SEK'.                  
001400*                                                                         
002000*                                                                         
007200     05  FILLER          REDEFINES WWPRIS01-IDMARKBO-GRUPP.               
007300         10  FILLER            OCCURS 7.                                  
007400             15  WWPRIS01-IDMARKBO      PIC X(1).                         
007500             15  WWPRIS01-KDVALISO-MC   PIC X(3).                         
007500             15  WWPRIS01-KDVALISO-COST PIC X(3).                         
007600*                                                                         
007700*** END COPY WWPRIS01  LENGTH=49                                          
      *KODEXEMPEL ATT FLYTTA DIT COPYTEXTEN ANV'NDS                             
      *                                                                         
      *  MOVE 1 TO IX                                                           
      *  PERFORM UNTIL IX > MAX-IX                                              
      *    IF WWPRIS01-IDMARKBO(IX) = WS-IDMARKBO                               
      *      MOVE WWPRIS01-KDVALISO-MC(IX)   TO WS-KDVALISO-MC                  
      *      MOVE WWPRIS01-KDVALISO-COST(IX) TO WS-KDVALISO-COST                
      *      MOVE MAX-IX                     TO IX                              
      *    END-IF                                                               
      *    ADD 1 TO IX                                                          
      *  END-PERFORM                                                            
      *  IF WS-KDVALISO-MC = SPACE                                              
      *    STRING 'MARKNADSBOLAG SAKNAS I WWPRIS01'                             
      *         DELIMITED BY SIZE INTO FELTEXT                                  
      *    DISPLAY FELTEXT                                                      
      *    CALL FELLOG                                                          
      *  END-IF                                                                 
      ***************                                                           
