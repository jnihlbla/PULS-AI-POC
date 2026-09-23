//V072963W JOB (510W4750100W475J055,W100),'TEST W475D5',                        
//             NOTIFY=V072963,PASSWORD=?,USER=VX79327,                          
//             CLASS=N,MSGCLASS=H                                               
/*JOBPARM LINES=999                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//*   /*JOBPARM FORMS=1800,LINECT=0                                             
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W475    EXEC W475TEST,STPLIB=F1ST00.SYPGM01B.LOAD                             
