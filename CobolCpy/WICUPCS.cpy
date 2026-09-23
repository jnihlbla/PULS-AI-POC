000010*** EDIT ALLOWED                                                          
000100******************************************************************        
000200* INFORMATION TILL ICU                                                    
000300******************************************************************        
000400* INTERACTIVE CHART UTILITY PANEL CONTROL STRUCTURE                       
000500******************************************************************        
000600*                                                                         
000700 01  ADMTPCS.                                                             
000800     02 PCSLEVEL            PIC S9(8)  COMP VALUE +1.                     
000900*         CHART UTILITY LEVEL IDENTIFIER                                  
001000*         1 = CHART UTILITY RELEASE 3 ONWARDS                             
001100     02 PCSDISP             PIC S9(8)  COMP VALUE +2.                     
001200*         CHART UTILITY FUNCTION REQUESTED                                
001300*         0 = SAVE CHART IN DATA FORMAT                                   
001400*         1 = SHOW HOME PANEL                                             
001500*         2 = DISPLAY, THEN HOME PANEL OR EXIT                            
001600*         3 = DISPLAY, BUT ONLY HELP, SAVE AND PRINT                      
001700*         4 = PRINT OR PLOT CHART                                         
001800*         5 = CONSTRUCT A CHART                                           
001900*         6 = CONSTRUCT A CHART                                           
002000*         7 = ONLY DISPLAY A CHART                                        
002100*         8 = ONLY SHOW THE DIRECTORY                                     
002200*         9 = CREATE A GDF FILE                                           
002300     02 PCSHELP             PIC S9(8)  COMP VALUE +1.                     
002400*         PF KEY INFORMATION SWITCH                                       
002500*         1 = PF INFORMATION IS DISPLAYED                                 
002600     02 PCSISOL             PIC S9(8)  COMP VALUE +0.                     
002700*         ISOLATION SWITCH                                                
002800*         0 = SAVE/RESTORE/DIRECTORY AVAILABLE                            
002900*         1 = SAVE/RESTORE/DIRECTORY NOT AVAILABLE                        
003000*         2 = SAVE/RESTORE AVAILABLE                                      
003100     02 PCSFNAME           PIC X(8) VALUE '*       '.                     
003200*         NAME OF PREVIOUSLY SAVED CHART FORMAT                           
003300*         * = DEFAULT CHART FORMAT                                        
003400     02 PCSDNAME           PIC X(8) VALUE '*       '.                     
003500*         NAME OF PREVIOUSLY SAVED CHART DATA                             
003600*         * = DATA CONTAINED IN PARAMETERS                                
003700     02 PCSPAIR            PIC S9(8)  COMP VALUE +0.                      
003800*         TIED OR FREE DATA SWITCH                                        
003900*         0 = TIED, ONE SET OF X-VALUES ONLY                              
004000     02 PCSNG              PIC S9(8) COMP VALUE +0.                       
004100*         NUMBER OF DATA GROUPS                                           
004200     02 PCSNE              PIC S9(8) COMP VALUE +0.                       
004300*         NUMBER OF ELEMENTS (PER DATA GROUP).                            
004400     02 PCSKEYL            PIC S9(8) COMP VALUE +11.                      
004500*         LENGTH OF 'KEYS' ARRAY ELEMENT                                  
004600     02 PCSLABL            PIC S9(8) COMP VALUE +4.                       
004700*         LENGTH OF 'LABELS' ARRAY ELEMENT                                
004800     02 PCSHEADL           PIC S9(8) COMP VALUE +30.                      
004900*         LENGTH OF 'HEADING' PARAMETER                                   
005000     02 PCSPNAME           PIC X(8)  VALUE '*       '.                    
005100*         LOCAL PRINTER DESTINATION NAME                                  
005200     02 PCSPRDEP           COMP-1 VALUE 0.0E+1.                           
005300*         PRINTER DEPTH (ROWS) OF CHART AREA                              
005400     02 PCSPRWID           COMP-1 VALUE 0.0E+1.                           
005500*         PRINTER WIDTH (COLS) OF CHART AREA                              
005600     02 PCSPCOPY           PIC S9(8)  COMP VALUE +1.                      
005700*         NUMBER OF COPIES OF PRINT FILE                                  
005800     02 PCSPHEAD           PIC S9(8)  COMP VALUE +1.                      
005900*         PRINTER HEADER PAGE CONTROL                                     
006000*         0 = DEFAULT, HEADER PAGE OUTPUT                                 
006100     02 PCSPVOFF           COMP-1 VALUE 0.0E+1.                           
006200*         PRINTER VERTICAL OFFSET (ROWS)                                  
006300     02 PCSPHOFF           COMP-1 VALUE 0.0E+1.                           
006400*         PRINTER HORIZONTAL OFFSET (COLUMNS)                             
006500     02 PCSPUNIT           PIC S9(8)  COMP VALUE +1.                      
006600*         PRINTER LAYOUT PARAMETER UNITS                                  
006700*         1 = PERCENT OF CORRESPANDING PAGE DIMENSION                     
006701*         2 = INCHES                                                      
006702*         3 = CENTIMETERS                                                 
006710*         4 = ROWS AND COLLUMNS                                           
006800     02 PCSDUMMY           PIC S9(8)  COMP VALUE +0.                      
006900*         RESERVED                                                        
007000     02 PCSDYNAM           PIC X(8) VALUE '       '.                      
007100*         DIRECTORY LISTING OBJECT NAME                                   
007200     02 PCSDYTYP           PIC S9(8)  COMP VALUE +0.                      
007300*         DIRECTORY LISTING OBJECT TYPE                                   
007400     02 PCSDYTYQ           PIC S9(8)  COMP VALUE +0.                      
007500*         DIRECTORY LISTING OBJECT SUBTYPE                                
007600     02 PCSDYLIB           PIC X(8) VALUE '       '.                      
007700*         DIRECTORY LISTING OBJECT LIBRARY                                
007800     02 PCSEXPL            PIC S9(8)  COMP VALUE +2.                      
007900*         INITIAL EXPERIENCE LEVEL VALUE                                  
008000*         0 = DEFAULT, STANDARD LEVEL                                     
008100*         1 = STANDARD LEVEL                                              
008200*         2 = ADVANCED LEVEL                                              
008300*** END COPY WICUPCSCC0  LENGTH=124   OLD LENGTH=                         
