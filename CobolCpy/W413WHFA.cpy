000100*** EDIT ALLOWED                                                          
010010 01  W413WHFA.                                                            
020011*                                                                         
030010*  TABELL FÖR HF-AK PLOCK.                                                
040000*                                                                         
050024     03  MAX-HFAK-IX   PIC S9(9)  VALUE 7 COMP SYNC.                      
051012*                                                                         
060012     03  HFAK-TAB.                                                        
061018         05  FILLER    PIC X(3)   VALUE 'STO'.                            
062010         05  FILLER    PIC X(2)   VALUE '02'.                             
063018         05  FILLER    PIC X(3)   VALUE '*QC'.                            
064010         05  FILLER    PIC X(2)   VALUE '03'.                             
065018         05  FILLER    PIC X(3)   VALUE 'SPX'.                            
066010         05  FILLER    PIC X(2)   VALUE '04'.                             
100018         05  FILLER    PIC X(3)   VALUE '*EP'.                            
110014         05  FILLER    PIC X(2)   VALUE '06'.                             
120024         05  FILLER    PIC X(3)   VALUE '#99'.                            
130021         05  FILLER    PIC X(2)   VALUE '07'.                             
131024         05  FILLER    PIC X(3)   VALUE '*ÖV'.                            
132024         05  FILLER    PIC X(2)   VALUE '07'.                             
140018         05  FILLER    PIC X(3)   VALUE 'REM'.                            
150015         05  FILLER    PIC X(2)   VALUE '07'.                             
290014     03  REHFAK REDEFINES HFAK-TAB.                                       
300024         05  FILLER                   OCCURS 7.                           
310017             07  HFAK-BERADREF        PIC X(3).                           
311025             07  FILLER REDEFINES HFAK-BERADREF.                          
312025                 09  HFAK-BERADREF-1  PIC X(1).                           
313025                 09  FILLER           PIC X(2).                           
320014             07  HFAK-ADLAGOMR        PIC X(2).                           
