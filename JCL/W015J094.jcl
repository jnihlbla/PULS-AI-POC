//W015J094 JOB (51091927000),'THOMAS E8 E9  ',                                  
//             MSGCLASS=H,MSGLEVEL=(1,1),                                       
//             CLASS=T,NOTIFY=W007184                                           
/*JOBPARM ROOM=HC2N,TIME=0,LINES=9,CARDS=0,FORMS=STD,LINECT=00                  
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W015     EXEC W015P094                                                        
//*                                                                             
