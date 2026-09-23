//W116SGRS JOB (640W1160100W116SGRS,W100),'RTN W116SG',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W116SG                                               
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN DD *                                                                  
 %WRTNIN2 W116.&VCOM..W11694 W116.W116SG.W11694                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116SGRS                                         
