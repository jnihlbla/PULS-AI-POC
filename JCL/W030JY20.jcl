//W030JY20 JOB (640W0010300W030JY20,W100),'RTN W030V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W030     EXEC W030P020,SYMB=YY                                                
//*                                                                             
//W030     EXEC W030P020,SYMB=OLDYY                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W030JY20                                         
