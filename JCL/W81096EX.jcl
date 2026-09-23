//W81096EX JOB (650W8100100W81096EX,W100),'RTN W810B1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=M                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ LOCAL                                                              
/*ROUTE PRINT LOCAL                                                             
//W481096 EXEC V335P030,                                                        
//            RUTIN=W810B1,MEMBER=W81096,                                       
//            INDSN='W810.W810B1.W81099(+0)',DISP='(OLD,KEEP,KEEP)',            
//**          UTDSN='W810.EXT.W81099',DEN=3,UNIT=T6                             
//            UTDSN='W810.EXT.W81099',DEN=3,UNIT=X9                             
//SOP     EXEC WSOPEND,PROCESS=W81096EX                                         
