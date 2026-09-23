//W473J002 JOB (650W4730100W473J002,W100),'RTN W473S1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM TIME=1,LINES=5,CARDS=0,FORMS=1800                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//        EXEC W473P002                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W473J002                                         
