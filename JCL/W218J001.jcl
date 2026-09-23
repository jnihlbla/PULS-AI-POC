//W218J001 JOB (640W2180100W218J001,W100),'RTN W218V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W218    EXEC W218P001                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W218J001                                         
